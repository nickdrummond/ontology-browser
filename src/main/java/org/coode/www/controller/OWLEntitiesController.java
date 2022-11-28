package org.coode.www.controller;

import org.coode.www.exception.OntServerException;
import org.coode.www.model.Characteristic;
import org.coode.www.model.OWLObjectWithOntology;
import org.coode.www.model.SearchResult;
import org.coode.www.model.SearchResults;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.NameService;
import org.coode.www.service.SearchService;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
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
            @RequestParam(required=true) final String name) throws OntServerException {

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
            @RequestParam(required=false) final Optional<String> property,
            final Model model) throws OntServerException {

        Optional<OWLAnnotationProperty> optAnnot = property.map(p -> kit.getOWLEntityChecker().getOWLAnnotationProperty(p));

        if (property.isPresent() && optAnnot.isEmpty()) {
            throw new OntServerException("Unknown property: " + property.get());
        }

        List<OWLObjectWithOntology> results = service.findByAnnotation(search, optAnnot, kit);

        if (results.size() == 1) {
            OWLObject owlObject = results.get(0).getOWLObject();
            if (owlObject instanceof OWLAnnotationAssertionAxiom) {
                OWLObject o = service.getEntities(((OWLAnnotationAssertionAxiom)owlObject).getSubject().asIRI().get(), kit).iterator().next();
                return "redirect:" + kit.getURLScheme().getURLForOWLObject(o);
            }
        }

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.empty());

        String propLabel = property.orElse("All annotations");

        Characteristic resultsCharacteristic = new Characteristic(null, propLabel, results);

        model.addAttribute("property", propLabel);
        model.addAttribute("search", search);
        model.addAttribute("results", resultsCharacteristic);
        model.addAttribute("mos", owlRenderer);

        return "searchresults";
    }
}
