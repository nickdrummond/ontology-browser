package org.coode.www.controller;

import org.coode.www.model.Bookmarks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpSession;

@Controller
public class RootController extends ApplicationController {

    @Autowired
    private Bookmarks bookmarks;

    @RequestMapping("/")
    public String index(final Model model) {
        model.addAttribute("application", applicationInfo);
        model.addAttribute("bookmarks", bookmarks.getBookmarks());
        model.addAttribute("doclet", "doclet-load");
        return "base";
    }

    @RequestMapping("/signout")
    public String signout(final Model model) {
        model.addAttribute("application", applicationInfo);
        model.addAttribute("doclet", "doclet-signout");
        return "base";
    }

    @RequestMapping("/signout-confirmed")
    public String signoutConfirmed(final HttpSession session) {
        session.invalidate();
        return "redirect:/";
    }
}
