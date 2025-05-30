package org.ontbrowser.www.feature.git;

import org.apache.commons.lang3.tuple.ImmutablePair;
import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.ListBranchCommand;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.diff.DiffEntry;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.revwalk.RevWalkUtils;
import org.eclipse.jgit.revwalk.filter.RevFilter;
import org.eclipse.jgit.treewalk.CanonicalTreeParser;
import org.eclipse.jgit.treewalk.FileTreeIterator;
import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.util.OWLUtils;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Profile;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

@Profile("git")
@Service
public class GitService implements BeforeLoad {

    private static final Logger log = LoggerFactory.getLogger(GitService.class);

    private final String remote;
    private final File local;
    private final String branch;

    public GitService(
            @Value("${git.remote}") String remote,
            @Value("${git.local}") String local,
            @Value("${git.branch}") String branch
    ) {
        this.local = new File(local);
        this.branch = branch;

        if (!this.local.exists()) {
            log.info("Creating repo directory: {}", this.local.getAbsolutePath());
            if (!this.local.mkdirs()) {
                throw new RuntimeException("Cannot create a directory for local git repo");
            }
        }

        if (!this.local.isDirectory()) {
            throw new RuntimeException("git repo must be a directory: " + this.local.getAbsolutePath() + " - specify correct GIT_LOCAL in ENV");
        }

        String foundRemote = null;

        try (Git git = Git.open(this.local)) {
            log.info("Found git repo at {}", this.local.getAbsolutePath());
            log.info("Fetching...");
            git.fetch().call();
            foundRemote = getRemoteURL(git).orElse(null);
            if (foundRemote != null) {
                if (remote != null && !Objects.equals(remote, foundRemote)) {
                    log.warn("Ignoring given git remote in favour of remote from local repo - {}", foundRemote);
                } else {
                    log.info("Local repo is tracking remote - {}", foundRemote);
                }
            }
        } catch (IOException e) {
            if (remote != null) {
                clone(remote, this.local, branch);
                foundRemote = remote;
            } else {
                throw new RuntimeException("No remote found - specify GIT_REMOTE in ENV");
            }
        } catch (GitAPIException e) {
            throw new RuntimeException(e);
        }
        this.remote = foundRemote;
    }

    @Scheduled(fixedRateString = "${git.refresh}")
    private void fetch() {
        withGit(git -> git.fetch().call());
    }

    // Work out the remote from the local repo
    private Optional<String> getRemoteURL(Git git) throws GitAPIException {
        return git.remoteList().call().stream()
                .filter(rc -> !rc.getURIs().isEmpty())
                .map(rc -> rc.getURIs().get(0).toString())
                .findFirst();
    }

    public void pull() {
        withGit(git -> {
            var remote = getRemote(git);
            remote.ifPresent(r -> {
                try {
                    log.info("Pulling from remote");
                    git.pull().call();
                } catch (GitAPIException e) {
                    throw new RuntimeException(e);
                }
            });
        });
    }


    // Expensive operation - only run on demand
    @Cacheable()
    public Map<String, DiffEntry.ChangeType> getChangedOntologies(
            Git git,
            Stream<OWLOntology> ontologies
    ) throws GitAPIException, IOException {

        List<DiffEntry> diffEntries = localChanges(git);

        return ontologies
                .map(ont -> new ImmutablePair<>(OWLUtils.ontIRI(ont), getDiff(ont, diffEntries)))
                .filter(pair -> pair.getRight().isPresent())
                .collect(Collectors.toMap(
                        ImmutablePair::getLeft,
                        pair -> pair.getRight().get().getChangeType())
                );

    }

    private Optional<DiffEntry> getDiff(OWLOntology ont, List<DiffEntry> diffEntries) {
        var docURI = ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString();
        if (docURI.startsWith("file:///")) {
            docURI = docURI.substring(7);
        }
        File file = new File(docURI);
        return isChanged(file, diffEntries);
    }

    @FunctionalInterface
    public interface CheckedConsumer<T> {
        void accept(T t) throws Exception;
    }

    public void withGit(CheckedConsumer<Git> actions) {
        try (Git git = open()) {
            actions.accept(git);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private void clone(String remote, File local, String branch) {
        try (Git git = Git.cloneRepository()
                .setURI(remote)
                .setDirectory(local)
                .setBranch(branch)
                .call()) {
            log.info("Cloned remote git repo {} to {}", remote, local.getAbsolutePath());
            log.info("Checking out {}", branch);
            var ref = git.checkout().setName(branch).call();
            log.info("Checked out {}", ref.getName());
        } catch (GitAPIException e) {
            throw new RuntimeException(e);
        }
    }

    public String getLocalBranchName(Git git) throws IOException {
        var localRepo = git.getRepository();
        return localRepo.getFullBranch();
    }

    public Ref getLocal(Git git) throws GitAPIException, IOException {
        var branchName = getLocalBranchName(git);
        List<Ref> branches = git.branchList().call();
        for (Ref localBranch : branches) {
            String localBranchName = localBranch.getName();
            if (localBranchName.equals(branchName)) {
                return localBranch;
            }
        }
        throw new RuntimeException("Cannot find local branch: " + branchName);
    }

    public Optional<Ref> getRemote(Git git) throws GitAPIException {
        List<Ref> branches = git.branchList()
                .setListMode(ListBranchCommand.ListMode.REMOTE)
                .setContains("remotes/origin/HEAD")
                .call();
        Optional<Ref> origin = branches.stream().findFirst();
        return origin;
    }

    public String getRev(Ref branchRef) {
        return branchRef.getObjectId().getName();
    }

    record RevStatus(List<RevCommit> ahead, List<RevCommit> behind) {
    }

    public RevStatus calculateDivergence(Git git, Ref local, Ref tracking) throws IOException {
        var localRepo = git.getRepository();
        try (RevWalk walk = new RevWalk(localRepo)) {
            RevCommit localCommit = walk.parseCommit(local.getObjectId());
            RevCommit trackingCommit = walk.parseCommit(tracking.getObjectId());
            walk.setRevFilter(RevFilter.MERGE_BASE);
            walk.markStart(localCommit);
            walk.markStart(trackingCommit);
            RevCommit mergeBase = walk.next();
            walk.reset();
            walk.setRevFilter(RevFilter.ALL);
            var aheadCommits = RevWalkUtils.find(walk, localCommit, mergeBase);
            var behindCommits = RevWalkUtils.find(walk, trackingCommit, mergeBase);
            return new RevStatus(aheadCommits, behindCommits);
        }
    }

    public List<DiffEntry> localChanges(Git git) throws GitAPIException, IOException {
        // Create the HEAD tree iterator
        var reader = git.getRepository().newObjectReader();
        var oldTreeIter = new CanonicalTreeParser();
        var headTree = git.getRepository().resolve("HEAD^{tree}");
        oldTreeIter.reset(reader, headTree);

        // Create the working tree iterator
        var newTreeIter = new FileTreeIterator(git.getRepository());
        // Either: call the diff command
        return git.diff()
                .setOldTree(oldTreeIter)
                .setNewTree(newTreeIter)
                .call();
    }

    public Optional<DiffEntry> isChanged(File file, List<DiffEntry> diff) {
        String localFile = getPathRelativeTo(file, this.local);
//        log.info("Checking for changes to {}", localFile);
        return diff.stream()
                .filter(d -> d.getNewPath().equals(localFile) || d.getOldPath().equals(localFile))
                .findFirst();
    }

    private String getPathRelativeTo(File file, File local) {
        var path = file.getAbsolutePath();
        var localPath = local.getAbsolutePath();
        if (path.startsWith(localPath)) {
            return path.substring(localPath.length() + 1);
        }
        return path;
    }

    public List<RevCommit> getCommits(Git git, int from, int size) throws GitAPIException {
        var commits = git.log().setMaxCount(size).setSkip(from).call();
        return StreamSupport.stream(commits.spliterator(), false).toList();
    }

    private Git open() throws IOException {
        return Git.open(local);
    }
}