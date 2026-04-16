package org.ontbrowser.www.feature.stats;

import org.ontbrowser.www.backend.BackendContext;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(value = "/stats")
public class StatsController {

    private final BackendContext backend;

    public StatsController(BackendContext backend) {
        this.backend = backend;
    }

    @GetMapping()
    public OntologyStats getStats(
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "EXCLUDED") final Imports imports
    ) {
        return backend.getStats().getOntologyStats(ont, imports);
    }
}
