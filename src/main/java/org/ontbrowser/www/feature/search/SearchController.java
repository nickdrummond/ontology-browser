package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping(value = "/entities")
public class SearchController {

    private final OWLHTMLKit kit;
    private final SearchService service;
    private final NameService nameService;

    public SearchController(
            OWLHTMLKit kit,
            SearchService service,
            NameService nameService
    ) {
        this.kit = kit;
        this.service = service;
        this.nameService = nameService;
    }

    @GetMapping(value = "/", produces = MediaType.APPLICATION_XML_VALUE)
    public SearchResults find(
            @ModelAttribute final OWLOntology ont,
            @RequestParam final String name,
            @RequestParam(defaultValue = "20") final int size) {

        var results = new SearchResults();

        // TODO use the ontology as the basis for the search

        // Minimum search length of 2
        if (name.length() > 1) {
            List<OWLEntity> entities = service.findByName(name, size, kit);
            for (OWLEntity owlEntity : entities) {
                String entityUrl = kit.getURLScheme().getURLForOWLObject(owlEntity, ont);
                String entityName = nameService.getName(owlEntity, kit);
                results.addResult(new SearchResult(entityUrl, "", entityName));
            }
        }

        return results;
    }
}
