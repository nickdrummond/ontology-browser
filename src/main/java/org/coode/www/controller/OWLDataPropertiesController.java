package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDataPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.OWLDataPropertyHierarchyService;
import org.coode.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Comparator;
import java.util.List;

@Controller
@RequestMapping(value="/dataproperties")
public class OWLDataPropertiesController extends ApplicationController {

    @Autowired
    private OWLDataPropertiesService service;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @GetMapping(value="/")
    public String getOWLDataProperties() {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();

        String id = service.getIdFor(owlTopDataProperty);

        return "redirect:/dataproperties/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}")
    public String getOWLDataProperty(
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

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/fragment")
    public String getOWLDataPropertyFragment(
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

        return "owlentityfragment";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/children")
    public String getChildren(
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

        return "base :: tree";
    }
}
