package org.coode.www.controller;

import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;

@Controller
public class CustomErrorController extends ApplicationController {

    @GetMapping("/error")
    public String handleError(
            HttpServletRequest httpRequest,
            Model model) {

        Set<OWLOntology> ontologies = kit.getOntologies();

        OWLOntology ont = kit.getActiveOntology();

        int code = (Integer) httpRequest.getAttribute("javax.servlet.error.status_code");

        model.addAttribute("activeOntology", ont);
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("errorCode", code);

        return "error";
    }
}