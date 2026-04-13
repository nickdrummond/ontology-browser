package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.backend.BackendContext;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(value = "/entities")
public class SearchController {

    private final BackendContext backend;
    private final SearchService service;

    public SearchController(
            BackendContext backend,
            SearchService service
    ) {
        this.backend = backend;
        this.service = service;
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
            var sfp = backend.getShortFormProvider();
            var finder = backend.getFinder();
            var entities = service.findByName(name, size, finder, sfp);
            for (OWLEntity owlEntity : entities) {
                var entityUrl = backend.getURLScheme().getURLForOWLObject(owlEntity, ont);
                var entityName = sfp.getShortForm(owlEntity);
                results.addResult(new SearchResult(entityUrl, "", entityName));
            }
        }

        return results;
    }
}
