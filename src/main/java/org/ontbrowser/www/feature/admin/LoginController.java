package org.ontbrowser.www.feature.admin;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.servlet.ModelAndView;

@Controller
public class LoginController extends ApplicationController {

    public LoginController(RestartableKit kit) {
        super(kit);
    }

    @GetMapping("/login")
    public ModelAndView login() {
        return new ModelAndView("login");
    }
}
