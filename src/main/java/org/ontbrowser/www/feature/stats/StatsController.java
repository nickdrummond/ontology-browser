package org.ontbrowser.www.feature.stats;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(value = "/stats")
public class StatsController {

    private final StatsService statsService;

    public StatsController(StatsService statsService) {
        this.statsService = statsService;
    }

    @GetMapping()
    public OntologyStats getStats(
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "EXCLUDED") final Imports imports
    ) {
        return statsService.getOntologyStats(ont, imports);
    }
}
