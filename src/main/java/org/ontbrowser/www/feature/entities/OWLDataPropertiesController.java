package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping(value = "/dataproperties")
public class OWLDataPropertiesController extends ApplicationController {

    private final OWLDataPropertiesService service;

    public OWLDataPropertiesController(
            OWLDataPropertiesService service) {
        this.service = service;
    }

    @GetMapping(value = "/")
    public void getOWLDataPropertiesOld(
            final HttpServletResponse response
    ) throws IOException {
        getOWLDataProperties(response);
    }

    @GetMapping()
    public void getOWLDataProperties(
            final HttpServletResponse response
    ) throws IOException {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        var owlTopDataProperty = df.getOWLTopDataProperty();

        String id = kit.lookup().getId(owlTopDataProperty);

        response.sendRedirect("/dataproperties/" + id);
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public ModelAndView getOWLDataProperty(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLDataProperty.class);

        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(prop));

        getOWLDataPropertyFragment(propertyId, pageSize, with, ont, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/fragment")
    public ModelAndView getOWLDataPropertyFragment(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLDataProperty.class);

        String entityName = kit.getShortFormProvider().getShortForm(prop);

        var owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(prop);

        List<With> withOrEmpty = with == null ? List.of() : with;

        var characteristics = service.getCharacteristics(prop, ont, kit.getComparator(), withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Data Property)");
        model.addAttribute("type", "Data Properties");
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

        var prop = kit.lookup().entityFor(propertyId, ont, OWLDataProperty.class);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));

        return new ModelAndView("base::children");
    }
}
