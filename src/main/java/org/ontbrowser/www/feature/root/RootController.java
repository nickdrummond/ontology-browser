package org.ontbrowser.www.feature.root;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.ontologies.OWLOntologiesController;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

@RestController
public class RootController extends ApplicationController {

    private final OWLOntologiesController ontologyController;

    public RootController(
            OWLHTMLKit kit,
            OWLOntologiesController ontologyController
    ) {
        super(kit);
        this.ontologyController = ontologyController;
    }

    enum IndexType {
        CLASSES_USAGE_CLOUD, INDIVIDUALS_USAGE_CLOUD, OBJECT_PROPERTIES_USAGE_CLOUD
    }

    @Value("${INDEX_TYPE:CLASSES_USAGE_CLOUD}")
    private IndexType indexType;

    // Entry point
    @GetMapping("/")
    public ModelAndView index(
            final Model model,
            @ModelAttribute OWLOntology ont,
            @RequestParam(required = false, defaultValue = "INCLUDED") Imports imports,
            @RequestParam(required = false) List<With> with,
            @RequestParam(required = false) final String redirect,
            HttpServletRequest request,
            HttpServletResponse response) throws IOException {

        if (redirect != null) {
            response.sendRedirect(redirect);
            return new ModelAndView();
        }
        else {
            model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));

            var ontologyId = String.valueOf(ont.getOntologyID().hashCode());
            ontologyController.getOntologyFragment(ontologyId, imports, with, model, request);

            return new ModelAndView("index");
        }
    }
}
