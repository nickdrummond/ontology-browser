package org.ontbrowser.www.controller;

import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.feature.stats.EntityCounts;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@ControllerAdvice
public class ReadOnlyOntologyControllerAdvice {

    private final ProjectInfo projectInfo;

    private final StatsService statsService;
    private final BackendContext backend;

    public ReadOnlyOntologyControllerAdvice(
            ProjectInfo projectInfo,
            StatsService statsService,
            BackendContext backend
    ) {
        this.projectInfo = projectInfo;
        this.statsService = statsService;
        this.backend = backend;
    }

    @ModelAttribute("projectInfo")
    public ProjectInfo getProjectInfo() {
        return projectInfo;
    }

    @ModelAttribute("mos")
    public OWLHTMLRenderer getRendererFactory(@RequestParam(required = false) final String ontId) {
        return new OWLHTMLRenderer(
                backend.getShortFormProvider(),
                backend.getOntologySFP(),
                backend.getIriShortFormProvider(),
                backend.getURLScheme(),
                getOnt(ontId),
                backend.getFinder()
        );
    }

    @ModelAttribute()
    public OWLOntology getOnt(@RequestParam(required = false) final String ontId) {
        if (ontId != null) {
            return backend.getOntologyFor(ontId);
        }
        return backend.getRootOntology();
    }

    @ModelAttribute("entityCounts")
    public EntityCounts getEntityCounts() {
        return statsService.getEntityCountsTotal();
    }
}
