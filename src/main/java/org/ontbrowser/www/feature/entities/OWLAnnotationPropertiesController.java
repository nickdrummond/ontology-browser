package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

import static org.springframework.http.HttpStatus.NOT_FOUND;

@RestController
@RequestMapping(value = "/annotationproperties")
public class OWLAnnotationPropertiesController extends ApplicationController {

    private final OWLAnnotationPropertiesService service;
    private CommonContent commonContent;

    public OWLAnnotationPropertiesController(
            OWLAnnotationPropertiesService service,
            CommonContent commonContent
    ) {
        this.service = service;
        this.commonContent = commonContent;
    }

    @GetMapping(value = "/")
    public void getOWLAnnotationPropertiesOld(
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response
    ) throws IOException {
        getOWLAnnotationProperties(ont, response);
    }

    @GetMapping()
    public void getOWLAnnotationProperties(
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response
    ) throws IOException {

        List<OWLAnnotationProperty> annotationProperties
                = service.getAnnotationProperties(ont, kit.getComparator());

        if (annotationProperties.isEmpty()) {
            throw new ResponseStatusException(NOT_FOUND, "Annotation properties not found");
        }

        OWLAnnotationProperty firstAnnotationProperty = annotationProperties.get(0);

        String id = kit.lookup().getId(firstAnnotationProperty);

        response.sendRedirect("/annotationproperties/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public ModelAndView getOWLAnnotationProperty(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        commonContent.addCommonContent(model, ont);
        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(prop));

        getOWLAnnotationPropertyFragment(propertyId, ont, pageSize, with, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/fragment")
    public ModelAndView getOWLAnnotationPropertyFragment(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        Comparator<OWLObject> comparator = kit.getComparator();

        String entityName = kit.getShortFormProvider().getShortForm(prop);

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(prop);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics =
                service.getCharacteristics(prop, ont, comparator, withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Annotation Property)");
        model.addAttribute("type", "Annotation Properties");
        model.addAttribute("iri", prop.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.name() + ": " + entityName);

        return new ModelAndView("owlentityfragment");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            final Model model
    ) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));

        return new ModelAndView("base::children");
    }
}
