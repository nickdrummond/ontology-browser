package org.coode.www.controller;

import org.coode.www.cloud.IndividualsByUsageCloud;
import org.coode.www.exception.OntServerException;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.Optional;
import java.util.Set;

@Controller
public class RootController extends ApplicationController {

    // required for refresh as the query controller has access to the reasoner(s) and results cache!
    @Autowired
    private DLQueryController queryController;

    // Entry point
    @RequestMapping("/")
    public String index(final Model model,
                        @RequestParam(required=false) final String redirect,
                        final HttpServletRequest request) throws OntServerException {

        if (redirect != null) {
            return "redirect:" + redirect;
        }
        else {
            IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(kit.getOntologies());

            CloudHelper<OWLNamedIndividual> helper = new CloudHelper<>(cloudModel);
            helper.setThreshold(14);
            helper.setZoom(4);
            helper.setNormalise(true);

            OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

            Set<OWLOntology> ontologies = kit.getOntologies();

            model.addAttribute("activeOntology", kit.getActiveOntology());
            model.addAttribute("ontologies", ontologies);
            model.addAttribute("cloud", cloudModel);
            model.addAttribute("helper", helper);
            model.addAttribute("mos", owlRenderer);

            return "index";
        }
    }
}
