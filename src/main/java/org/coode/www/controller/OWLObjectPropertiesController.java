package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.coode.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
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
import java.util.Comparator;
import java.util.List;

@Controller
@RequestMapping(value="/objectproperties")
public class OWLObjectPropertiesController extends ApplicationController {

    private static final String EMPTY_WITH = "";
    @Autowired
    private OWLObjectPropertiesService service;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @GetMapping(value="/")
    public String getOWLObjectProperties() {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = service.getIdFor(owlTopObjectProperty);

        return "redirect:/objectproperties/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}")
    public String getOWLObjectProperty(
        @PathVariable final String propertyId,
        @RequestParam(required = false) List<With> with,
        final HttpServletRequest request,
        final Model model
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLObjectProperty property = service.getPropertyFor(propertyId, ont);

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLObjectPropertyHierarchyService hierarchyService =
                new OWLObjectPropertyHierarchyService(r, comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getPrunedTree(property);

        model.addAttribute("hierarchy", prunedTree);

        getOWLObjectPropertyFragment(propertyId, with, request, model);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/fragment/{propertyId}")
    public String getOWLObjectPropertyFragment(
        @PathVariable final String propertyId,
        @RequestParam(required = false) List<With> with,
        final HttpServletRequest request,
        final Model model
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLObjectProperty property = service.getPropertyFor(propertyId, ont);

        String entityName = kit.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(property);

        List<With> withOrEmpty = with == null ? List.of() : with;

        List<Characteristic> characteristics = service.getCharacteristics(property, ont, kit.getComparator(), withOrEmpty, 30);

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Object Properties");
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        return "owlentityfragment";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/children")
    public String getChildren(
        @PathVariable final String propertyId,
        final Model model
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLObjectProperty property = service.getPropertyFor(propertyId, ont);

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(r, comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
