package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLAnnotationPropertiesService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Controller
@RequestMapping(value="/annotationproperties")
public class OWLAnnotationPropertiesController extends ApplicationController {

    @Autowired
    private OWLAnnotationPropertiesService service;

    @GetMapping(value="/")
    public String getOWLAnnotationProperties()
            throws NotFoundException {

        OWLOntology activeOntology = kit.getActiveOntology();

        List<OWLAnnotationProperty> annotationProperties
                = service.getAnnotationProperties(activeOntology, kit.getComparator());

        if (annotationProperties.isEmpty()) {
            throw new NotFoundException("Annotation properties", "");
        }

        OWLAnnotationProperty firstAnnotationProperty = annotationProperties.get(0);

        String id = service.getIdFor(firstAnnotationProperty);

        return "redirect:/annotationproperties/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}")
    public String getOWLAnnotationProperty(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLOntology activeOntology = kit.getActiveOntology();

        Comparator<OWLObject> comparator = kit.getComparator();

        List<OWLAnnotationProperty> annotationProperties =
                service.getAnnotationProperties(activeOntology, comparator);

        model.addAttribute("entities", annotationProperties);

        getOWLAnnotationPropertyFragment(propertyId, pageSize, with, model, request, response);

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/fragment")
    public String getOWLAnnotationPropertyFragment(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLOntology activeOntology = kit.getActiveOntology();

        OWLAnnotationProperty owlAnnotationProperty = service.getPropertyFor(propertyId, activeOntology);

        Comparator<OWLObject> comparator = kit.getComparator();

        String entityName = kit.getShortFormProvider().getShortForm(owlAnnotationProperty);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(activeOntology).withActiveObject(owlAnnotationProperty);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics =
                service.getCharacteristics(owlAnnotationProperty, activeOntology, comparator, withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Annotation Property)");
        model.addAttribute("type", "Annotation Properties");
        model.addAttribute("iri", owlAnnotationProperty.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.getName() + ": " + entityName);

        return "owlentityfragment";
    }
}
