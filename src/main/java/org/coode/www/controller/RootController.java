package org.coode.www.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;

@Controller
public class RootController extends ApplicationController {

    @RequestMapping("/")
    public String index(final Model model) {
        model.addAttribute("application", applicationInfo);
        model.addAttribute("doclet", "doclet-load");
        return "base";
    }
}
