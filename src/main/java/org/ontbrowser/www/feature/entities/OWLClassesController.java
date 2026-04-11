package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping(value = "/classes")
public class OWLClassesController {

    private final OWLHTMLKit kit;
    private final OWLClassesService service;
    private final ReasonerFactoryService reasonerFactoryService;
    private final StatsService statsService;
    private final CommonContent commonContent;
    private final CommonFragments commonFragments;

    public OWLClassesController(
            OWLHTMLKit kit,
            OWLClassesService service,
            ReasonerFactoryService reasonerFactoryService,
            StatsService statsService,
            CommonContent commonContent,
            CommonFragments commonFragments) {
        this.kit = kit;
        this.service = service;
        this.reasonerFactoryService = reasonerFactoryService;
        this.statsService = statsService;
        this.commonContent = commonContent;
        this.commonFragments = commonFragments;
    }

    @GetMapping(value = "/")
    public void getOWLClassesOld(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response) throws IOException {
        getOWLClasses(ontId, response);
    }

    @GetMapping()
    public void getOWLClasses(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {

        OWLClass owlThing = kit.getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        String id = kit.lookup().getId(owlThing);

        response.sendRedirect("/classes/" + id+ (ontId != null ? "?ontId=" + ontId : ""));
    }

    @GetMapping(value = "/{classId}")
    public ModelAndView getOWLClass(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "classDescendants") final String statsName,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var owlClass = kit.lookup().entityFor(classId, ont, OWLClass.class);

        commonContent.addCommonContent(request.getQueryString(), model, ont);
        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(owlClass));
        model.addAttribute("stats", statsService.getClassStats(statsName, reasonerFactoryService.getToldReasoner(ont)));
        model.addAttribute("statsName", statsName);

        getOWLClassFragment(classId, ont, with, model, request);

        return new ModelAndView("owlentity");
    }

    @GetMapping(value = "/{classId}/fragment")
    public ModelAndView getOWLClassFragment(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var owlClass = kit.lookup().entityFor(classId, ont, OWLClass.class);
        return commonFragments.getOWLClassFragment(service, owlClass, false, ont, with, model, request.getQueryString());
    }

    @GetMapping(value = "/{classId}/children")
    public ModelAndView getChildren(
            @PathVariable final String classId,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        var owlClass = kit.lookup().entityFor(classId, ont, OWLClass.class);

        var r = reasonerFactoryService.getToldReasoner(ont);
        var stats = statsService.getClassStats(statsName, r);

        return commonFragments.getClassChildren(owlClass, r, stats, model);
    }
}
