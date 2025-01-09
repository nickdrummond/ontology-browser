package org.coode.www.oi;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.BranchConfig;
import org.eclipse.jgit.lib.ObjectReader;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.revwalk.RevWalkUtils;
import org.eclipse.jgit.revwalk.filter.RevFilter;
import org.eclipse.jgit.transport.FetchResult;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.List;
import java.util.Map;

record RevStatus(int ahead, int behind){}


// TODO could configure the browser to track a git repo

// could warn in UI that updates available, or show revision/history etc
// allow switch version????
// Downside is disk space, initial clone, unnecessary additional files/history etc
// starwars is 1.3G
public class GitOntologyStore {

    public static void main(String[] args) throws GitAPIException {
        String remote = "https://github.com/nickdrummond/star-wars-ontology.git";
        File directory = new File("./git-repos/starwars/");

        Git git = get(remote, directory);

        checkStatus(remote, git);

        git.close();
    }

    private static void checkStatus(String remote, Git git) throws GitAPIException {
        final Repository localRepo = git.getRepository();

        // update status of remote
        fetch(git, remote);

        // TODO can we get the remote Ref without doing the lsRemoteRepo?
        Map<String, Ref> remotes = Git.lsRemoteRepository()
                .setRemote(remote)
                .callAsMap();

        List<Ref> branches = git.branchList().call();
        for (Ref branch : branches) {
            System.out.println("branch = " + branch);
            Ref r = remotes.get(branch.getName());
            System.out.println("remote = " + r);

            // None of the below work
//            System.out.println("rrrrr = " + localRepo.getRemoteName(branch.getName()));
//            System.out.println("branch.getLeaf() = " + branch.getStorage());
//            BranchConfig branchConfig = new BranchConfig(localRepo.getConfig(), branch.getName());
//            System.out.println("branchConfig = " + branchConfig);
//            System.out.println("branchConfig.getPushRemote() = " + branchConfig.getPushRemote());
//            String trackingBranch = branchConfig.getTrackingBranch();
//            System.out.println("trackingBranch = " + trackingBranch);

            try {
                RevStatus status = calculateDivergence(localRepo, branch, r);
                System.out.println("status = " + status);
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }

    private static Git get(String remote, File directory) throws GitAPIException {
        Git git = null;
        try {
            git = Git.open(directory);
        } catch (IOException e) {
            System.out.println("No repo at " + directory + ": " + e.getMessage());
            System.out.println("checking out from " + remote);
            git = Git.cloneRepository()
                .setURI(remote)
                .setDirectory(directory)
                .call();
        }
        return git;
    }

    // TODO must be able to work ou the remote from the git repo
    private static void fetch(Git git, String remote) throws GitAPIException {
        FetchResult result = git.fetch().setRemote(remote)
                .setRefSpecs("refs/heads/master:refs/remotes/origin/master")
                .call();
        if (!result.getMessages().isEmpty()) {
            System.out.println("result = " + result.getMessages());
        }
        System.out.println("Fetched from " + result.getURI());
    }

    private static RevStatus calculateDivergence(Repository repository, Ref local, Ref tracking) throws IOException {
        try (RevWalk walk = new RevWalk(repository)) {
            RevCommit localCommit = walk.parseCommit(local.getObjectId());
            System.out.println("localCommit = " + localCommit);
            RevCommit trackingCommit = walk.parseCommit(tracking.getObjectId());
            System.out.println("trackingCommit = " + trackingCommit);
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

}
