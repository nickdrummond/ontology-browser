package org.ontbrowser.www.feature.stats;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping(value = "/stats")
public class StatsController extends ApplicationController {

    private final StatsService statsService;

    public StatsController(
            OWLHTMLKit kit,
            StatsService statsService) {
        super(kit);
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
