package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

import static openllet.core.utils.Namespaces.OWL;

@RestController
@RequestMapping(value = "/classes")
public class OWLClassesController {

    private final BackendContext backend;
    private final OWLClassesService service;
//    private final ReasonerFactoryService reasonerFactoryService;
    private final StatsService statsService;
    private final CommonContent commonContent;
    private final CommonFragments commonFragments;

    public OWLClassesController(
            BackendContext backend,
            OWLClassesService service,
//            ReasonerFactoryService reasonerFactoryService,
            StatsService statsService,
            CommonContent commonContent,
            CommonFragments commonFragments) {
        this.backend = backend;
        this.service = service;
//        this.reasonerFactoryService = reasonerFactoryService;
        this.statsService = statsService;
        this.commonContent = commonContent;
        this.commonFragments = commonFragments;
    }

    @GetMapping(value = "/")
    public void getOWLClassesOld(
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request) throws IOException {
        getOWLClass(null, ont, null, null, model, request);
    }

//    @GetMapping()
//    public void getOWLClasses(
//            @RequestParam(required = false) final String ontId,
//            final HttpServletResponse response
//    ) throws IOException {
//        response.sendRedirect("/classes?iri=" + OWLRDFVocabulary.OWL_THING + (ontId != null ? "&ontId=" + ontId : ""));
//    }

    @GetMapping()
    public ModelAndView getOWLClass(
            @RequestParam(defaultValue = OWL + "Thing") final String iri,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "classDescendants") final String statsName,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var owlClass = backend.getOWLDataFactory().getOWLClass(iri);

        commonContent.addCommonContent(request.getQueryString(), model, ont);

        var toldReasoner = backend.getToldReasoner(ont);

        model.addAttribute("hierarchy", service.getHierarchyService(ont, toldReasoner).getPrunedTree(owlClass));
        model.addAttribute("stats", statsService.getClassStats(statsName, toldReasoner));
        model.addAttribute("statsName", statsName);

        getOWLClassFragment(iri, ont, with, model, request);

        return new ModelAndView("owlentity");
    }

    @GetMapping(value = "/fragment")
    public ModelAndView getOWLClassFragment(
            @RequestParam final String iri,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var owlClass = backend.getOWLDataFactory().getOWLClass(iri);
        return commonFragments.getOWLClassFragment(service, owlClass, false, ont, with, model, request.getQueryString());
    }

    @GetMapping(value = "/children")
    public ModelAndView getChildren(
            @RequestParam final String iri,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        var owlClass = backend.getOWLDataFactory().getOWLClass(iri);

        var r = backend.getToldReasoner(ont);
        var stats = statsService.getClassStats(statsName, r);

        return commonFragments.getClassChildren(owlClass, r, stats, model);
    }
}
