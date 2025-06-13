package org.ontbrowser.www.feature.search;

import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.OntServerException;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping(value = "/entities")
public class FindByAnnotationController extends ApplicationController {

    private final FindByAnnotation service;

    public FindByAnnotationController(FindByAnnotation service) {
        this.service = service;
    }

    @GetMapping(value = "annotation")
    public ModelAndView findAnnotation(
            @RequestParam final String search,
            @RequestParam(required = false) final String property,
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response,
            final Model model) throws OntServerException, IOException {

        // TODO use the ontology as a basis for search

        Optional<String> optProp = Optional.ofNullable(property);

        Optional<OWLAnnotationProperty> optAnnot = optProp.map(p -> kit.getOWLEntityChecker().getOWLAnnotationProperty(p));

        if (optProp.isPresent() && optAnnot.isEmpty()) {
            throw new OntServerException("Unknown property: " + property);
        }

        List<AxiomWithMetadata> results = service.findByAnnotation(search, optAnnot.orElse(null), kit);

        if (results.size() == 1) {
            OWLObject owlObject = results.get(0).owlObject();
            if (owlObject instanceof OWLAnnotationAssertionAxiom ax) {
                Optional<IRI> iri = ax.getSubject().asIRI();
                OWLObject o = ont.getEntitiesInSignature(iri.orElseThrow(), Imports.INCLUDED).iterator().next();
                response.sendRedirect(kit.getURLScheme().getURLForOWLObject(o, ont));
                return null;
            }
        }

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont);

        String propLabel = optProp.orElse("All annotations");

        Characteristic resultsCharacteristic = new Characteristic(null, propLabel, results);

        model.addAttribute("property", propLabel);
        model.addAttribute("search", search);
        model.addAttribute("results", resultsCharacteristic);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("searchresults");
    }
}
