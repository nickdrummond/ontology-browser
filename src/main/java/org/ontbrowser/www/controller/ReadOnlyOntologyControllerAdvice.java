package org.ontbrowser.www.controller;

import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.feature.stats.EntityCounts;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.renderer.RendererFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@ControllerAdvice
public class ReadOnlyOntologyControllerAdvice {

    private final ProjectInfo projectInfo;

    private final RendererFactory rendererFactory;
    private final StatsService statsService;
    private final BackendContext backendContext;

    public ReadOnlyOntologyControllerAdvice(
            ProjectInfo projectInfo,
            RendererFactory rendererFactory,
            StatsService statsService,
            BackendContext backendContext
    ) {
        this.projectInfo = projectInfo;
        this.rendererFactory = rendererFactory;
        this.statsService = statsService;
        this.backendContext = backendContext;
    }

    @ModelAttribute("projectInfo")
    public ProjectInfo getProjectInfo() {
        return projectInfo;
    }

    @ModelAttribute("mos")
    public OWLHTMLRenderer getRendererFactory(@RequestParam(required = false) final String ontId) {
        return rendererFactory.getHTMLRenderer(getOnt(ontId));
    }

    @ModelAttribute()
    public OWLOntology getOnt(@RequestParam(required = false) final String ontId) {
        if (ontId != null) {
            return backendContext.getOntologyFor(ontId);
        }
        return backendContext.getRootOntology();
    }

    @ModelAttribute("entityCounts")
    public EntityCounts getEntityCounts() {
        return statsService.getEntityCountsTotal();
    }
}
