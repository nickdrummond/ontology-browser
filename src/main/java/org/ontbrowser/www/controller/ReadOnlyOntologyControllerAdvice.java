package org.ontbrowser.www.controller;

import org.ontbrowser.www.feature.ontologies.OWLOntologiesService;
import org.ontbrowser.www.feature.stats.EntityCounts;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.renderer.RendererFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;

@ControllerAdvice
@Profile("!editing")
public class ReadOnlyOntologyControllerAdvice {

    private final OWLHTMLKit kit;

    private final ProjectInfo projectInfo;
    private final RendererFactory rendererFactory;

    private final OWLOntologiesService ontService;
    private final StatsService statsService;

    public ReadOnlyOntologyControllerAdvice(
            OWLHTMLKit kit,
            ProjectInfo projectInfo,
            RendererFactory rendererFactory,
            OWLOntologiesService ontService,
            StatsService statsService
    ) {
        this.kit = kit;
        this.projectInfo = projectInfo;
        this.rendererFactory = rendererFactory;
        this.ontService = ontService;
        this.statsService = statsService;
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
            return ontService.getOntologyFor(ontId, kit);
        }
        return kit.getRootOntology();
    }

    @ModelAttribute("entityCounts")
    public EntityCounts getEntityCounts() {
        return statsService.getEntityCountsTotal();
    }
}
