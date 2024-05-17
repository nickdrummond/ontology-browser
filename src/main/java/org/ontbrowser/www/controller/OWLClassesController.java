package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLClassesService;
import org.ontbrowser.www.service.ReasonerFactoryService;
import org.ontbrowser.www.service.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

@RestController
@RequestMapping(value="/classes")
public class OWLClassesController extends ApplicationController {

    private final OWLClassesService service;
    private final ReasonerFactoryService reasonerFactoryService;

    public OWLClassesController(
            @Autowired OWLClassesService service,
            @Autowired ReasonerFactoryService reasonerFactoryService) {
        this.service = service;
        this.reasonerFactoryService = reasonerFactoryService;
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
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, ont);

        Comparator<Tree<OWLClass>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, comparator);

        Tree<OWLClass> prunedTree = hierarchyService.getPrunedTree(owlClass);

        model.addAttribute("hierarchy", prunedTree);

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

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlClass);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = service.getCharacteristics(
                owlClass, ont, kit.getComparator(),
                withOrEmpty,
                DEFAULT_PAGE_SIZE);

        Set<OWLClass> namedSuperclasses = service.getNamedTypes(owlClass, ont);

        String supers = String.join(", ", namedSuperclasses.stream().map(sfp::getShortForm).toList());

        String title = entityName + (supers.isEmpty() ? "" : " (" + supers + ")");

        model.addAttribute("title", title);
        model.addAttribute("type", "Classes");
        model.addAttribute("iri", owlClass.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.getName() + ": " + title);
        return new ModelAndView("owlentityfragment");

    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/children")
    public ModelAndView getChildren(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            final Model model) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, ont);

        Comparator<Tree<OWLClass>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, comparator);

        Tree<OWLClass> prunedTree = hierarchyService.getChildren(owlClass);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base::tree");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/instances")
    public ModelAndView getInstances(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            final Model model) throws NotFoundException {

        OWLEntity owlClass = service.getOWLClassFor(classId, ont);

        Comparator<Tree<OWLEntity>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLIndividualsByTypeHierarchyService hierarchyService = new OWLIndividualsByTypeHierarchyService(r, comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getChildren(owlClass);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base::tree");
    }
}
