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
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
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
                results.addResult(new SearchResult(kit.getURLScheme().getURLForOWLObject(owlEntity).toString(), "", nameService.getName(owlEntity, kit)));
            }
        }

        return results;
    }

    @RequestMapping(value="annotation", method=RequestMethod.GET)
    public String findAnnotation(
            @RequestParam final String search,
            @RequestParam(required=false) final Optional<String> property,
            final HttpServletRequest request) throws OntServerException {
        return redirect(request);
    }
}
