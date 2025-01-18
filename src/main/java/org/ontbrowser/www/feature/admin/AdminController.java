package org.ontbrowser.www.feature.admin;

import org.ontbrowser.www.controller.ApplicationController;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

@Profile("admin")
@RestController
@RequestMapping(value="/admin")
public class AdminController extends ApplicationController {

    @GetMapping()
    public ModelAndView admin() {
        return new ModelAndView("admin");
    }
}
