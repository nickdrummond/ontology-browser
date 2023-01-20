package org.coode.www.controller;

import org.coode.www.exception.OntServerException;
import org.coode.www.model.Characteristic;
import org.coode.www.model.OWLObjectWithOntology;
import org.coode.www.model.SearchResult;
import org.coode.www.model.SearchResults;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.NameService;
import org.coode.www.service.SearchService;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import java.util.List;
import java.util.Optional;

@Controller
@RequestMapping(value="/entities")
public class OWLEntitiesController extends ApplicationController {

    @Autowired
    private SearchService service;

    @Autowired
    private NameService nameService;

    @RequestMapping(value="/", method=RequestMethod.GET, produces = MediaType.APPLICATION_XML_VALUE)
    public @ResponseBody SearchResults find(
            @RequestParam final String name) {

        SearchResults results = new SearchResults();

        // Minimum search length of 2
        if (name.length() > 1) {
            List<OWLEntity> entities = service.findByName(name, kit);
            for (OWLEntity owlEntity : entities) {
                results.addResult(new SearchResult(kit.getURLScheme().getURLForOWLObject(owlEntity), "", nameService.getName(owlEntity, kit)));
            }
        }

        return results;
    }

    @RequestMapping(value="annotation", method=RequestMethod.GET)
    public String findAnnotation(
            @RequestParam final String search,
            @RequestParam(required = false) final String property,
            final Model model) throws OntServerException {

        Optional<String> optProp = Optional.ofNullable(property);

        Optional<OWLAnnotationProperty> optAnnot = optProp.map(p -> kit.getOWLEntityChecker().getOWLAnnotationProperty(p));

        if (optProp.isPresent() && optAnnot.isEmpty()) {
            throw new OntServerException("Unknown property: " + property);
        }

        List<OWLObjectWithOntology> results = service.findByAnnotation(search, optAnnot.orElse(null), kit);

        if (results.size() == 1) {
            OWLObject owlObject = results.get(0).getOWLObject();
            if (owlObject instanceof OWLAnnotationAssertionAxiom) {
                Optional<IRI> iri = ((OWLAnnotationAssertionAxiom) owlObject).getSubject().asIRI();
                OWLObject o = service.getEntities(iri.orElseThrow(), kit).iterator().next();
                return "redirect:" + kit.getURLScheme().getURLForOWLObject(o);
            }
        }

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        String propLabel = optProp.orElse("All annotations");

        Characteristic resultsCharacteristic = new Characteristic(null, propLabel, results);

        model.addAttribute("property", propLabel);
        model.addAttribute("search", search);
        model.addAttribute("results", resultsCharacteristic);
        model.addAttribute("mos", owlRenderer);

        return "searchresults";
    }
}
