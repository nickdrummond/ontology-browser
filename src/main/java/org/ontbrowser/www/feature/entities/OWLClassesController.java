package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping(value = "/classes")
public class OWLClassesController {

    private final BackendContext backend;
    private final OWLClassesService service;
    private final CommonContent commonContent;
    private final CommonFragments commonFragments;

    public OWLClassesController(
            BackendContext backend,
            OWLClassesService service,
            CommonContent commonContent,
            CommonFragments commonFragments) {
        this.backend = backend;
        this.service = service;
        this.commonContent = commonContent;
        this.commonFragments = commonFragments;
    }


    @GetMapping()
    public void getOWLClasses(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {
        String id = backend.getIriShortFormProvider().getShortForm(OWLRDFVocabulary.OWL_THING.getIRI());
        response.sendRedirect("/classes/" + id + (ontId != null ? "?ontId=" + ontId : ""));
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
        var owlClass = backend.lookup().entityFor(classId, ont, OWLClass.class);

        commonContent.addCommonContent(request.getQueryString(), model, ont);

        var toldReasoner = backend.getToldReasoner(ont);

        model.addAttribute("hierarchy", service.getHierarchyService(ont, toldReasoner).getPrunedTree(owlClass));
        model.addAttribute("stats", backend.getStats().getClassStats(statsName, toldReasoner));
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
        var owlClass = backend.lookup().entityFor(classId, ont, OWLClass.class);
        return commonFragments.getOWLClassFragment(service, owlClass, false, ont, with, model, request.getQueryString());
    }

    @GetMapping(value = "/{classId}/children")
    public ModelAndView getChildren(
            @PathVariable final String classId,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        var owlClass = backend.lookup().entityFor(classId, ont, OWLClass.class);

        var r = backend.getToldReasoner(ont);
        var stats = backend.getStats().getClassStats(statsName, r);

        return commonFragments.getClassChildren(owlClass, r, stats, model);
    }
}
