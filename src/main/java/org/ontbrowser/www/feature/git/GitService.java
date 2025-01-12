package org.ontbrowser.www.feature.git;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.revwalk.RevWalkUtils;
import org.eclipse.jgit.revwalk.filter.RevFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.StreamSupport;

// TODO
// Should not load ontology until REPO is cloned!!!
// warning if the repo is behind
// pull and reload
// allow switch version????
// Periodic refresh (fetch)

@Profile("git")
@Service
public class GitService {

    private static final Logger log = LoggerFactory.getLogger(GitService.class);

    private final String remote;
    private final File local;

    public GitService(
            @Value("${git.remote}") String remote,
            @Value("${git.local}") String local
    ) {
        this.remote = remote;
        this.local = new File(local);

        if (!this.local.exists()) {
            throw new RuntimeException("git repo does not exist: " + this.local.getAbsolutePath() + " - specify correct GIT_LOCAL in ENV");
        }

        if (!this.local.isDirectory()) {
            throw new RuntimeException("git repo must be a directory: " + this.local.getAbsolutePath() + " - specify correct GIT_LOCAL in ENV");
        }

        try (Git git = Git.open(this.local)) {
            log.info("Found git repo at {}", this.local.getAbsolutePath());
        } catch (IOException e) {
            if (remote != null) {
                clone(remote, this.local);
            }
            else {
                throw new RuntimeException("No remote found - specify GIT_REMOTE in ENV");
            }
        }
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

    private void clone(String remote, File local) {
        try (Git git = Git.cloneRepository()
                .setURI(remote)
                .setDirectory(local)
                .call()){
            log.info("Cloned remote git repo {} to {}", remote, local.getAbsolutePath());
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

    public Optional<Ref> getRemote(Git git) throws GitAPIException, IOException {
        // TODO can we get the remote Ref without doing the lsRemoteRepo?
        Map<String, Ref> remotes = Git.lsRemoteRepository()
                .setRemote(remote)
                .callAsMap();

        return Optional.ofNullable(remotes.get(getLocalBranchName(git)));
    }

    public String getRev(Ref branchRef) {
        return branchRef.getObjectId().getName();
    }

    record RevStatus(int ahead, int behind){}

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
            int aheadCount = RevWalkUtils.count(walk, localCommit, mergeBase);
            int behindCount = RevWalkUtils.count(walk, trackingCommit, mergeBase);
            return new RevStatus(aheadCount, behindCount);
        }
    }

    public List<RevCommit> getCommits(Git git, int from, int size) throws GitAPIException {
        var commits = git.log().setMaxCount(size).setSkip(from).call();
        return StreamSupport.stream(commits.spliterator(), false).toList();
    }

    private Git open() throws IOException {
        return Git.open(local);
    }

//    public void getStatus(Git git) {
//        git.
//    }
}