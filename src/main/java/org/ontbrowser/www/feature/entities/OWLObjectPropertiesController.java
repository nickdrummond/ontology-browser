package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.graph.GraphURLScheme;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

@RestController
@RequestMapping(value = "/objectproperties")
public class OWLObjectPropertiesController extends ApplicationController {

    private final OWLObjectPropertiesService service;
    private final CommonContent commonContent;

    public OWLObjectPropertiesController(
            OWLObjectPropertiesService service, CommonContent commonContent) {
        this.service = service;
        this.commonContent = commonContent;
    }

    @GetMapping(value = "/")
    public void getOWLObjectPropertiesOld(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {
        getOWLObjectProperties(ontId, response);
    }

    @GetMapping()
    public void getOWLObjectProperties(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = kit.lookup().getId(owlTopObjectProperty);

        response.sendRedirect("/objectproperties/" + id+ (ontId != null ? "?ontId=" + ontId : ""));
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public ModelAndView getOWLObjectProperty(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        commonContent.addCommonContent(request, model, ont);
        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(prop));

        getOWLObjectPropertyFragment(propertyId, ont, with, model, request, response);

        return new ModelAndView("owlentity");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/fragment")
    public ModelAndView getOWLObjectPropertyFragment(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        String entityName = kit.getShortFormProvider().getShortForm(prop);

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(prop);

        List<With> withOrEmpty = with == null ? List.of() : with;

        List<Characteristic> characteristics = service.getCharacteristics(prop, ont, kit.getComparator(), withOrEmpty, 30);

        if (projectInfo.activeProfiles().contains("graph")) {
            var mos = new MOSStringRenderer(kit.getFinder(), ont);
            model.addAttribute("graphLink", new GraphURLScheme(mos).getURLForOWLObject(prop, ont));
        }

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Object Properties");
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
        var prop = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));

        return new ModelAndView("tree::children");
    }
}
