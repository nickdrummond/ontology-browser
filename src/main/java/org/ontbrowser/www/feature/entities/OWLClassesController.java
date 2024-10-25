package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.ReasonerFactoryService;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.service.stats.Stats;
import org.ontbrowser.www.service.stats.StatsService;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping(value="/classes")
public class OWLClassesController extends ApplicationController {

    private final OWLClassesService service;
    private final ReasonerFactoryService reasonerFactoryService;
    private final StatsService statsService;
    private final ReasonerService reasonerService;

    public OWLClassesController(
            @Autowired OWLClassesService service,
            @Autowired ReasonerFactoryService reasonerFactoryService,
            @Autowired StatsService statsService,
            @Autowired ReasonerService reasonerService) {
        this.service = service;
        this.reasonerFactoryService = reasonerFactoryService;
        this.statsService = statsService;
        this.reasonerService = reasonerService;
    }

    private CommonFragments getCommon() {
        return new CommonFragments(kit, projectInfo, reasonerService);
    }

    @GetMapping(value="/")
    public void getOWLClasses(
            final HttpServletResponse response
    ) throws IOException {

        OWLClass owlThing = kit.getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        String id = service.getIdFor(owlThing);

        response.sendRedirect("/classes/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}")
    public ModelAndView getOWLClass(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam (defaultValue = "classDescendants") final String statsName,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, ont);

        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(owlClass));
        model.addAttribute("stats", statsService.getClassStats(statsName, reasonerFactoryService.getToldReasoner(ont)));
        model.addAttribute("statsName", statsName);

        getOWLClassFragment(classId, ont, with, model, request, response);

        return new ModelAndView("owlentity");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/fragment")
    public ModelAndView getOWLClassFragment(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, ont);
        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlClass);
        return getCommon().getOWLClassFragment(service, owlClass, ont, owlRenderer, with, model, request, response);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/children")
    public ModelAndView getChildren(
            @PathVariable final String classId,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, ont);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);
        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);
        Stats stats = statsService.getClassStats(statsName, r);

        return getCommon().getClassChildren(owlClass, r, owlRenderer, stats, model);
    }
}
