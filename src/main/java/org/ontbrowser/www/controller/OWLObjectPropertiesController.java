package org.ontbrowser.www.controller;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLObjectPropertiesService;
import org.ontbrowser.www.service.ReasonerFactoryService;
import org.ontbrowser.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.Comparator;
import java.util.List;

@RestController
@RequestMapping(value="/objectproperties")
public class OWLObjectPropertiesController extends ApplicationController {

    private final OWLObjectPropertiesService service;
    private final ReasonerFactoryService reasonerFactoryService;

    public OWLObjectPropertiesController(
            @Autowired OWLObjectPropertiesService service,
            @Autowired ReasonerFactoryService reasonerFactoryService) {
        this.service = service;
        this.reasonerFactoryService = reasonerFactoryService;
    }

    @GetMapping(value="/")
    public void getOWLObjectProperties(
            final HttpServletResponse response
    ) throws IOException {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = service.getIdFor(owlTopObjectProperty);

        response.sendRedirect("/objectproperties/" + id);
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}")
    public ModelAndView getOWLObjectProperty(
        @PathVariable final String propertyId,
        @ModelAttribute final OWLOntology ont,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLObjectProperty prop = service.getPropertyFor(propertyId, ont);

        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(prop));

        getOWLObjectPropertyFragment(propertyId, ont, with, model, request, response);

        return new ModelAndView("owlentity");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/fragment")
    public ModelAndView getOWLObjectPropertyFragment(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

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

        response.addHeader("title", projectInfo.getName() + ": " + entityName);

        return new ModelAndView("owlentityfragment");

    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/children")
    public ModelAndView getChildren(
        @PathVariable final String propertyId,
        @ModelAttribute final OWLOntology ont,
        final Model model
    ) throws NotFoundException {

        OWLObjectProperty prop = service.getPropertyFor(propertyId, ont);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("mos", rendererFactory.getRenderer(ont));

        return new ModelAndView("base::children");
    }
}
