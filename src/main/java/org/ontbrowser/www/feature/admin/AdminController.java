package org.ontbrowser.www.feature.admin;

import org.ontbrowser.www.backend.memory.kit.impl.RestartableKit;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

@Profile("admin")
@RestController
@RequestMapping(value="/admin")
@ConditionalOnProperty(name = "ontology.backend", havingValue = "memory", matchIfMissing = true) // mem only
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
