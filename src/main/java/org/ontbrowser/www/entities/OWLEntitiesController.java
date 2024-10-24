package org.ontbrowser.www.entities;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.OntServerException;
import org.ontbrowser.www.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.SearchResult;
import org.ontbrowser.www.model.SearchResults;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.NameService;
import org.ontbrowser.www.service.SearchService;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping(value="/entities")
public class OWLEntitiesController extends ApplicationController {

    private final SearchService service;
    private final NameService nameService;

    public OWLEntitiesController(
            @Autowired SearchService service,
            @Autowired NameService nameService) {
        this.service = service;
        this.nameService = nameService;
    }

    @GetMapping(value="/", produces = MediaType.APPLICATION_XML_VALUE)
    public SearchResults find(
            @ModelAttribute final OWLOntology ont,
            @RequestParam final String name) {

        SearchResults results = new SearchResults();

        // TODO use the ontology as the basis for the search

        // Minimum search length of 2
        if (name.length() > 1) {
            List<OWLEntity> entities = service.findByName(name, kit);
            for (OWLEntity owlEntity : entities) {
                results.addResult(new SearchResult(kit.getURLScheme().getURLForOWLObject(owlEntity), "", nameService.getName(owlEntity, kit)));
            }
        }

        return results;
    }

    @GetMapping(value="annotation")
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

        List<AxiomWithMetadata> results = service.findByAnnotation(search, optAnnot.orElse(null), ont);

        if (results.size() == 1) {
            OWLObject owlObject = results.get(0).getOWLObject();
            if (owlObject instanceof OWLAnnotationAssertionAxiom ax) {
                Optional<IRI> iri = ax.getSubject().asIRI();
                OWLObject o = service.getEntities(iri.orElseThrow(), ont).iterator().next();
                response.sendRedirect(kit.getURLScheme().getURLForOWLObject(o));
                return new ModelAndView("");
            }
        }

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        String propLabel = optProp.orElse("All annotations");

        Characteristic resultsCharacteristic = new Characteristic(null, propLabel, results);

        model.addAttribute("property", propLabel);
        model.addAttribute("search", search);
        model.addAttribute("results", resultsCharacteristic);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("searchresults");
    }
}
