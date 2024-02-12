package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;

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
    public String getOWLAnnotationProperty(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLOntology activeOntology = kit.getActiveOntology();

        Comparator<OWLObject> comparator = kit.getComparator();

        List<OWLAnnotationProperty> annotationProperties =
                service.getAnnotationProperties(activeOntology, comparator);

        model.addAttribute("entities", annotationProperties);

        getOWLAnnotationPropertyFragment(propertyId, model);

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/fragment/{propertyId}")
    public String getOWLAnnotationPropertyFragment(@PathVariable final String propertyId,
                                           final Model model) throws NotFoundException {

        OWLOntology activeOntology = kit.getActiveOntology();

        OWLAnnotationProperty owlAnnotationProperty = service.getPropertyFor(propertyId, activeOntology);

        Comparator<OWLObject> comparator = kit.getComparator();

        String entityName = kit.getShortFormProvider().getShortForm(owlAnnotationProperty);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(activeOntology).withActiveObject(owlAnnotationProperty);

        List<Characteristic> characteristics = service.getCharacteristics(owlAnnotationProperty, activeOntology, comparator);

        model.addAttribute("title", entityName + " (Annotation Property)");
        model.addAttribute("type", "Annotation Properties");
        model.addAttribute("iri", owlAnnotationProperty.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentityfragment";
    }
}
