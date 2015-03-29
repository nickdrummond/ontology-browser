package org.coode.www.controller;

import org.coode.www.model.Bookmarks;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

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
}
