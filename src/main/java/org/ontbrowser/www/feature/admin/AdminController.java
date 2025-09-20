package org.ontbrowser.www.feature.admin;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

@Profile("admin")
@RestController
@RequestMapping(value="/admin")
public class AdminController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(AdminController.class);

    public AdminController(OWLHTMLKit kit) {
        super(kit);
    }

    @GetMapping()
    public ModelAndView admin() {
        return new ModelAndView("admin");
    }

    @GetMapping("/restart")
    public ModelAndView restart() {
        var model = new ModelAndView("admin");
        try {
            kit.restart();
        } catch (OWLOntologyCreationException e) {
            log.error("Cannot restart", e);
            model.setStatus(HttpStatus.INTERNAL_SERVER_ERROR);
        }
        return model;
    }
}
