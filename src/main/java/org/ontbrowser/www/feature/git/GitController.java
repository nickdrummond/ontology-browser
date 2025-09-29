package org.ontbrowser.www.feature.git;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.ontbrowser.www.model.paging.PageData;
import org.ontbrowser.www.url.GlobalPagingURIScheme;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.security.Principal;
import java.util.Objects;

@RestController
@Profile("git")
@RequestMapping(value = "/git")
public class GitController {

    private static final Logger log = LoggerFactory.getLogger(GitController.class);

    private final GitService gitService;
    private final RestartableKit kit;

    public GitController(
            RestartableKit kit,
            GitService gitService) {
        this.kit = kit;
        this.gitService = gitService;
    }

    @GetMapping("/update")
    public void update(
            HttpServletResponse response
    ) throws IOException, OWLOntologyCreationException {
        if (gitService.pull()) {
            kit.restart();
        }
        response.sendRedirect("/git/history");
    }

    @GetMapping("/history")
    public ModelAndView history(
            @RequestParam(defaultValue = "0") int start,
            @RequestParam(defaultValue = "20") int pageSize,
            Model model,
            HttpServletRequest request,
            Principal principal
    ) {
        if (!gitService.isAvailable()) {
            return new ModelAndView("redirect:/");
        }

        gitService.withGit(git -> {
            var local = gitService.getLocal(git);
            var commits = gitService.getCommits(git, start, pageSize);

            var remote = gitService.getRemote(git);
            var status = "not tracking remote";
            if (remote.isPresent()) {
                model.addAttribute("remote", remote.get());

                var changedOntologies = gitService.getChangedOntologies(git, kit.getOWLOntologyManager().ontologies());
                model.addAttribute("changedOntologies", changedOntologies);

                if (Objects.equals(gitService.getRev(local), gitService.getRev(remote.get()))) {
                    status = "that is up to date";
                } else {
                    status = "with updates available";
                    var divergence = gitService.calculateDivergence(git, local, remote.get());
                    log.info("divergence: {}", divergence);
                    model.addAttribute("divergence", divergence);
                }
            }
            if (isAdmin(principal)) {
                model.addAttribute("refresh", true);
            }

            model.addAttribute("status", status);
            model.addAttribute("local", local.getName());
            model.addAttribute("commits", commits);
            model.addAttribute("pageURIScheme", new GlobalPagingURIScheme(request.getQueryString()));
            model.addAttribute("pageData", new PageData(start, pageSize, Integer.MAX_VALUE));
        });
        return new ModelAndView("history");
    }

    private boolean isAdmin(Principal principal) {
        if (principal instanceof UsernamePasswordAuthenticationToken token) {
            var authorities = token.getAuthorities();
            if (!authorities.isEmpty()) {
                return authorities.stream().anyMatch(auth -> auth.getAuthority().equals("ROLE_ADMIN"));
            }
        }
        return false;
    }
}
