package org.ontbrowser.www.feature.admin;

import org.ontbrowser.www.kit.impl.RestartableKit;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

@Profile("admin")
@RestController
@RequestMapping(value="/admin")
public class AdminController {

    private final RestartableKit kit;

    public AdminController(RestartableKit kit) {
        this.kit = kit;
    }

    @GetMapping()
    public ModelAndView admin() {
        return new ModelAndView("admin");
    }

    @GetMapping("/restart")
    public ModelAndView restart() {
        var model = new ModelAndView("admin");
        kit.restart();
        return model;
    }
}
