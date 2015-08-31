package org.coode.www.controller;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Bookmarks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.SessionAttributes;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

@Controller
@SessionAttributes("kit")
public class RootController extends ApplicationController {

    @Autowired
    private Bookmarks bookmarks;

    // Entry point for session creation and by default load ontologies page
    @RequestMapping("/")
    public String index(final Model model,
                        @RequestParam(required=false) final String label,
                        @RequestParam(required=false) final String redirect,
                        final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        model.addAttribute("kit", kit);

        if (redirect != null) {
            return "redirect:" + redirect;
        }
        else {
            model.addAttribute("options", optionsService.getConfig(kit));
            model.addAttribute("bookmarks", bookmarks.getBookmarks());

            return "load";
        }
    }

    @RequestMapping("/signout")
    public String signout(final Model model,
                          final HttpServletRequest request) throws OntServerException {

        OWLHTMLKit kit = sessionManager.getHTMLKit(request);

        model.addAttribute("options", optionsService.getConfig(kit));
        model.addAttribute("activeOntology", kit.getActiveOntology());
        model.addAttribute("ontologies", kit.getOntologies());
        return "signout";
    }

    @RequestMapping("/signout-confirmed")
    public String signoutConfirmed(final HttpSession session) {
        session.invalidate();
        return "redirect:/";
    }
}
