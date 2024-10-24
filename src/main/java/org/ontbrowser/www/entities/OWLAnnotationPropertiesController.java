package org.ontbrowser.www.entities;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@RestController
@RequestMapping(value="/annotationproperties")
public class OWLAnnotationPropertiesController extends ApplicationController {

    private final OWLAnnotationPropertiesService service;

    public OWLAnnotationPropertiesController(
            @Autowired OWLAnnotationPropertiesService service) {
        this.service = service;
    }

    @GetMapping(value="/")
    public void getOWLAnnotationProperties(
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response
    ) throws IOException, NotFoundException {

        List<OWLAnnotationProperty> annotationProperties
                = service.getAnnotationProperties(ont, kit.getComparator());

        if (annotationProperties.isEmpty()) {
            throw new NotFoundException("Annotation properties", "");
        }

        OWLAnnotationProperty firstAnnotationProperty = annotationProperties.get(0);

        String id = service.getIdFor(firstAnnotationProperty);

        response.sendRedirect("/annotationproperties/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}")
    public ModelAndView getOWLAnnotationProperty(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        var prop = service.getPropertyFor(propertyId, ont);

        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(prop));

        getOWLAnnotationPropertyFragment(propertyId, ont, pageSize, with, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/fragment")
    public ModelAndView getOWLAnnotationPropertyFragment(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLAnnotationProperty owlAnnotationProperty = service.getPropertyFor(propertyId, ont);

        Comparator<OWLObject> comparator = kit.getComparator();

        String entityName = kit.getShortFormProvider().getShortForm(owlAnnotationProperty);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlAnnotationProperty);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics =
                service.getCharacteristics(owlAnnotationProperty, ont, comparator, withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Annotation Property)");
        model.addAttribute("type", "Annotation Properties");
        model.addAttribute("iri", owlAnnotationProperty.getIRI());
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

        OWLAnnotationProperty prop = service.getPropertyFor(propertyId, ont);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("mos", rendererFactory.getRenderer(ont));

        return new ModelAndView("base::children");
    }
}
