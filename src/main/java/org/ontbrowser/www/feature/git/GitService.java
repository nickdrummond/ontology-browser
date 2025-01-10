package org.ontbrowser.www.feature.git;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.revwalk.RevCommit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.StreamSupport;

// TODO
// Should not load ontology until REPO is cloned!!!
// warning if the repo is behind
// pull and reload
// allow switch version????

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

    public List<RevCommit> getCommits(int from, int size) {
        try (Git git = open()) {
            var commits = git.log().setMaxCount(size).setSkip(from).call();
            return StreamSupport.stream(commits.spliterator(), false).toList();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private Git open() throws IOException {
        return Git.open(local);
    }
}