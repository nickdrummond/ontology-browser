package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLDataPropertiesService;
import org.ontbrowser.www.service.ReasonerFactoryService;
import org.ontbrowser.www.service.hierarchy.OWLDataPropertyHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Comparator;
import java.util.List;

@RestController
@RequestMapping(value="/dataproperties")
public class OWLDataPropertiesController extends ApplicationController {

    private final OWLDataPropertiesService service;
    private final ReasonerFactoryService reasonerFactoryService;

    public OWLDataPropertiesController(
            @Autowired OWLDataPropertiesService service,
            @Autowired ReasonerFactoryService reasonerFactoryService) {
        this.service = service;
        this.reasonerFactoryService = reasonerFactoryService;
    }

    @GetMapping(value="/")
    public void getOWLDataProperties(
            final HttpServletResponse response
    ) throws IOException {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();

        String id = service.getIdFor(owlTopDataProperty);

        response.sendRedirect("/dataproperties/" + id);
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}")
    public ModelAndView getOWLDataProperty(
        @PathVariable final String propertyId,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLDataProperty owlDataProperty = service.getOWLDataPropertyFor(propertyId, kit);

        Comparator<Tree<OWLDataProperty>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntology ont = kit.getActiveOntology();

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLDataPropertyHierarchyService hierarchyService = new OWLDataPropertyHierarchyService(r, comparator);

        Tree<OWLDataProperty> prunedTree = hierarchyService.getPrunedTree(owlDataProperty);

        model.addAttribute("hierarchy", prunedTree);

        getOWLDataPropertyFragment(propertyId, pageSize, with, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/fragment")
    public ModelAndView getOWLDataPropertyFragment(
        @PathVariable final String propertyId,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLDataProperty owlDataProperty = service.getOWLDataPropertyFor(propertyId, kit);

        OWLOntology ont = kit.getActiveOntology();

        String entityName = kit.getShortFormProvider().getShortForm(owlDataProperty);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlDataProperty);

        List<With> withOrEmpty = with == null ? List.of() : with;

        List<Characteristic> characteristics = service.getCharacteristics(owlDataProperty, ont, kit.getComparator(), withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Data Property)");
        model.addAttribute("type", "Data Properties");
        model.addAttribute("iri", owlDataProperty.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.getName() + ": " + entityName);

        return new ModelAndView("owlentityfragment");

    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            final Model model
    ) throws NotFoundException {

        OWLDataProperty property = service.getOWLDataPropertyFor(propertyId, kit);

        Comparator<Tree<OWLDataProperty>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntology ont = kit.getActiveOntology();

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLDataPropertyHierarchyService hierarchyService = new OWLDataPropertyHierarchyService(r, comparator);

        Tree<OWLDataProperty> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base :: tree");
    }
}
