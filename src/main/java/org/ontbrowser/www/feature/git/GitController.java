package org.ontbrowser.www.feature.git;

import jakarta.servlet.http.HttpServletRequest;
import org.eclipse.jgit.revwalk.RevCommit;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Profile;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.util.List;
import java.util.Objects;

@RestController
@Profile("git")
@RequestMapping(value = "/git")
public class GitController extends ApplicationController {

    private final GitService gitService;

    public GitController(@Autowired GitService gitService) {
        this.gitService = gitService;
    }

    @GetMapping("/history")
    public ModelAndView history(
            @RequestParam(defaultValue = "0") int start,
            @RequestParam(defaultValue = "20") int pageSize,
            Model model,
            HttpServletRequest request
    ) {
        gitService.withGit(git -> {
            var local = gitService.getLocal(git);
            var commits = gitService.getCommits(git, start, pageSize);

            var remote = gitService.getRemote(git);
            var status = "not tracking remote";
            if (remote.isPresent()) {
                model.addAttribute("remote", remote.get());

                if (Objects.equals(gitService.getRev(local), gitService.getRev(remote.get()))) {
                    status = "up to date";
                }
                else {
                    status = "updates available";
                    var divergence = gitService.calculateDivergence(git, local, remote.get());
                    model.addAttribute("divergence", divergence);
                }
            }
            model.addAttribute("status", status);
            model.addAttribute("local", local);
            model.addAttribute("commits", commits);
            model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));
            model.addAttribute("pageData", new PageData(start, pageSize, Integer.MAX_VALUE));
        });
        return new ModelAndView("history");
    }
}