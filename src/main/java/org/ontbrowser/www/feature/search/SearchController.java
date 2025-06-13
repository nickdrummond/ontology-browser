package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.controller.ApplicationController;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = "/entities")
public class SearchController extends ApplicationController {

    private final SearchService service;
    private final NameService nameService;

    public SearchController(
            SearchService service,
            NameService nameService) {
        this.service = service;
        this.nameService = nameService;
    }

    @GetMapping(value = "/", produces = MediaType.APPLICATION_XML_VALUE)
    public SearchResults find(
            @ModelAttribute final OWLOntology ont,
            @RequestParam final String name,
            @RequestParam(defaultValue = "20") final int size) {

        SearchResults results = new SearchResults();

        // TODO use the ontology as the basis for the search

        // Minimum search length of 2
        if (name.length() > 1) {
            List<OWLEntity> entities = service.findByName(name, size, kit);
            for (OWLEntity owlEntity : entities) {
                results.addResult(new SearchResult(kit.getURLScheme().getURLForOWLObject(owlEntity, ont), "", nameService.getName(owlEntity, kit)));
            }
        }

        return results;
    }
}
