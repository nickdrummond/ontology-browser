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
        List<RevCommit> commits = gitService.getCommits(start, pageSize);
        model.addAttribute("commits", commits);
        model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request));
        model.addAttribute("pageData", new PageData(start, pageSize, Integer.MAX_VALUE));
        return new ModelAndView("history");
    }
}
