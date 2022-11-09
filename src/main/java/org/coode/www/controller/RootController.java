package org.coode.www.controller;

import org.coode.www.cloud.IndividualsByUsageCloud;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.SessionAttributes;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.Optional;
import java.util.Set;

@Controller
public class RootController extends ApplicationController {

    // Entry point
    @RequestMapping("/")
    public String index(final Model model,
                        @RequestParam(required=false) final String redirect,
                        final HttpServletRequest request) throws OntServerException {
        return redirect(request);
    }

    @RequestMapping("/refresh")
    public String refresh(final HttpSession session,
                          final HttpServletRequest request) throws OWLOntologyCreationException {
        return redirect(request);
    }
}
