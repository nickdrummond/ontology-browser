package org.coode.www.controller;

import com.google.common.base.Optional;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@Controller
@RequestMapping(value="/annotationproperties")
@SessionAttributes("kit")
public class OWLAnnotationPropertiesController extends ApplicationController {

    @Autowired
    private OWLAnnotationPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLAnnotationProperties(@ModelAttribute("kit") final OWLHTMLKit kit)
            throws OntServerException, NotFoundException {

        OWLServer owlServer = kit.getOWLServer();

        OWLOntology activeOntology = owlServer.getActiveOntology();

        List<OWLAnnotationProperty> annotationProperties
                = service.getAnnotationProperties(activeOntology, owlServer.getComparator());

        OWLAnnotationProperty firstAnnotationProperty = annotationProperties.get(0);

        String id = service.getIdFor(firstAnnotationProperty);

        return "redirect:/annotationproperties/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLAnnotationProperty(@PathVariable final String propertyId,
                                           @ModelAttribute("kit") final OWLHTMLKit kit,
                              final Model model) throws OntServerException, NotFoundException {

        OWLAnnotationProperty owlAnnotationProperty = service.getOWLAnnotationPropertyFor(propertyId, kit);

        OWLServer owlServer = kit.getOWLServer();

        OWLOntology activeOntology = owlServer.getActiveOntology();

        List<OWLAnnotationProperty> annotationProperties =
                service.getAnnotationProperties(activeOntology, owlServer.getComparator());

        String entityName = owlServer.getShortFormProvider().getShortForm(owlAnnotationProperty);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.of(owlAnnotationProperty));

        model.addAttribute("title", entityName + " (Annotation Property)");
        model.addAttribute("type", "Annotation Properties");
        model.addAttribute("iri", owlAnnotationProperty.getIRI());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", activeOntology);
        model.addAttribute("ontologies", owlServer.getOntologies());
        model.addAttribute("entities", annotationProperties);
        model.addAttribute("characteristics", service.getCharacteristics(owlAnnotationProperty, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }
}
