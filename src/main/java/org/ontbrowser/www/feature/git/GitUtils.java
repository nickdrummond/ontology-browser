package org.ontbrowser.www.feature.git;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.Ref;
import org.eclipse.jgit.lib.Repository;
import org.eclipse.jgit.revwalk.RevCommit;
import org.eclipse.jgit.revwalk.RevWalk;
import org.eclipse.jgit.revwalk.RevWalkUtils;
import org.eclipse.jgit.revwalk.filter.RevFilter;
import org.eclipse.jgit.transport.FetchResult;

import java.io.IOException;
import java.util.List;
import java.util.Map;

public class GitUtils {

    record RevStatus(int ahead, int behind){}

    private static void checkStatus(String remoteRepo, Git git) throws GitAPIException, IOException {
        final Repository localRepo = git.getRepository();

        // update status of remote
        fetch(git, remoteRepo);

        // TODO can we get the remote Ref without doing the lsRemoteRepo?
        Map<String, Ref> remotes = Git.lsRemoteRepository()
                .setRemote(remoteRepo)
                .callAsMap();

        final String currentCheckedOutBranch = localRepo.getFullBranch();
        System.out.println("currentCheckedOutBranch = " + currentCheckedOutBranch);


        List<Ref> branches = git.branchList().call();
        System.out.println(branches.size() + " local branches");
        for (Ref localBranch : branches) {
            String localBranchName = localBranch.getName();
            if (localBranchName.equals(currentCheckedOutBranch)) {
                final String localRev = localBranch.getObjectId().getName();

                Ref remote = remotes.get(localBranchName);
                if (remote == null) {
                    System.out.println("Local branch is not tracking a remote");
                }
                else {
                    String remoteRev = remote.getObjectId().getName();
                    String remoteBranchName = remote.getName();
                    System.out.println("tracking remote = " + remoteBranchName + " rev " + remoteRev);

                    if (!localRev.equals(remoteRev)) {
                        System.out.println("Out of sync. Updating...");
                        // why do I need to fetch again?
                        git.fetch().call();
                        RevStatus status = calculateDivergence(localRepo, localBranch, remote);
                        System.out.println("status = " + status);
                        if (status.behind() > 0) {
                            System.out.println("pulling " + status.behind() + " commits");
                            // TODO reenable the pull
//                            PullResult result = git.pull().call();
//                            if (result.isSuccessful()) {
//                                System.out.println("result = " + result.getMergeResult().toString());
//                            }
                        }
                    }
                }
            }
        }
    }

    // TODO must be able to work out the remote from the git repo
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
