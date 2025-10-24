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
import org.ontbrowser.www.kit.Config;
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
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

/**
 * Simply checks if the root ontology is in a git repo, and if so, uses that.
 */
@Profile("git")
@Service
public class GitService implements BeforeLoad {

    private static final Logger log = LoggerFactory.getLogger(GitService.class);

    private String remote;
    private File local;
    private final String branch;

    public GitService(
            @Value("${ontology.root.location}") String ontologyRoot,
            @Value("${git.branch}") String branch
    ) {
        this.branch = branch;
    }

    private void findRepo(String ontologyRoot) {
        var repoContainingRoot = findGitRepoFromFile(new File(ontologyRoot));
        if (repoContainingRoot.isPresent()) {
            this.local = repoContainingRoot.get();

            try (Git git = Git.open(this.local)) {
                log.info("Found git repo at {}", this.local.getAbsolutePath());
                log.info("Fetching...");
                git.fetch().call();
                String foundRemote = getRemoteURL(git).orElse(null);
                if (foundRemote != null) {
                    log.info("Local repo is tracking remote - {}", foundRemote);
                }
                this.remote = foundRemote;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        else {
            log.warn("Local git repo at {} not found", ontologyRoot);
            this.local = null;
            this.remote = null;
        }
    }

    public boolean isAvailable() {
        return local != null;
    }

    @Scheduled(fixedRateString = "${git.refresh}")
    private void fetch() {
        try {
            withGit(git -> git.fetch().call());
        }
        catch (Exception e) {
            log.warn("Error refreshing git: " + e.getMessage());
        }
    }

    // Work out the remote from the local repo
    private Optional<String> getRemoteURL(Git git) throws GitAPIException {
        return git.remoteList().call().stream()
                .filter(rc -> !rc.getURIs().isEmpty())
                .map(rc -> rc.getURIs().get(0).toString())
                .findFirst();
    }

    public boolean pull() {
        if (local == null) {
            return false;
        }
        withGit(git -> {
            var remote = getRemote(git);
            if(remote.isPresent()) {
                try {
                    log.info("Pulling from remote");
                    git.pull().call();
                } catch (GitAPIException e) {
                    throw new RuntimeException(e);
                }
            }
        });
        return true;
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

    @Override
    public void beforeLoad(Config config) {
        findRepo(config.root());
    }

    @FunctionalInterface
    public interface CheckedConsumer<T> {
        void accept(T t) throws Exception;
    }

    public void withGit(CheckedConsumer<Git> actions) {
        if (local == null) {
            return;
        }
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

    private Optional<File> findGitRepoFromFile(File file) {
        try {
            File directory = file.isDirectory() ? file : file.getParentFile();
            org.eclipse.jgit.storage.file.FileRepositoryBuilder builder =
                    new org.eclipse.jgit.storage.file.FileRepositoryBuilder()
                            .setMustExist(true)
                            .addCeilingDirectory(new File("/"))
                            .findGitDir(directory);

            if (builder.getGitDir() != null) {
                // Return the parent of .git directory which is the repository root
                return Optional.of(builder.getGitDir().getParentFile());
            }
            return Optional.empty();
        } catch (Exception e) {
            log.warn("Could not find git repository for file: {}", file, e);
            return Optional.empty();
        }
    }
}