package org.coode.www.controller;

import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;

@Controller
public class CustomErrorController extends ApplicationController {

    private static final Logger log = LoggerFactory.getLogger(CustomErrorController.class);

    @GetMapping("/error")
    public String handleError(
            HttpServletRequest httpRequest,
            Model model) {

        Set<OWLOntology> ontologies = kit.getOntologies();

        OWLOntology ont = kit.getActiveOntology();

        Object errorCode = httpRequest.getAttribute("javax.servlet.error.status_code");
        if (errorCode instanceof Integer code) {
            model.addAttribute("errorCode", code);
        }
        else {
            log.warn("Unknown error code {} : {}", errorCode.getClass(), errorCode);
            model.addAttribute("errorCode", 0);
        }

        model.addAttribute("activeOntology", ont);
        model.addAttribute("ontologies", ontologies);

        return "error";
    }
}