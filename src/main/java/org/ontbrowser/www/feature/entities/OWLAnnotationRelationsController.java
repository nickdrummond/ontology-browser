package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.List;
import java.util.Set;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE_STR;
import static org.springframework.http.HttpStatus.NOT_FOUND;

@RestController
@RequestMapping(value = "/relations/" + OWLAnnotationRelationsController.PATH)
public class OWLAnnotationRelationsController {

    public static final String PATH = "onannotationproperty";
    public static final String RELATION_TEMPLATE = "relation";

    private final BackendContext backend;
    private final OWLAnnotationPropertiesService propertiesService;
    private final OWLIndividualsService individualsService;
    private final CommonRelations<OWLAnnotationProperty> commonRelations;
    private final CommonFragments commonFragments;

    public OWLAnnotationRelationsController(
            BackendContext backend,
            OWLAnnotationPropertiesService propertiesService,
            OWLIndividualsService individualsService,
            CommonFragments commonFragments
    ) {
        this.backend = backend;
        this.propertiesService = propertiesService;
        this.individualsService = individualsService;
        this.commonRelations = new CommonRelations<>(
                PATH,
                backend,
                propertiesService
        );
        this.commonFragments = commonFragments;
    }


    @GetMapping(value = "/")
    public void getRelationsForAnnotationPropertyOld(
            final HttpServletResponse response
    ) throws IOException {
        getRelationsForAnnotationProperty(response);
    }

    @GetMapping()
    public void getRelationsForAnnotationProperty(
            final HttpServletResponse response
    ) throws IOException {
        Set<OWLAnnotationProperty> props = backend.getRootOntology().getAnnotationPropertiesInSignature(Imports.INCLUDED);
        if (props.isEmpty()) {
            throw new ResponseStatusException(NOT_FOUND, "Annotation Properties not found");
        }
        // Start with random
        String id = backend.getIriShortFormProvider().getShortForm(props.iterator().next().getIRI());
        response.sendRedirect("/relations/" + PATH + "/" + id);
    }


    @GetMapping(value = "/{annotationPropertyId}")
    public ModelAndView getRelationsForAnnotationProperty(
            @PathVariable final String annotationPropertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(defaultValue = "annotationsCount") final String statsName,
            final Model model,
            HttpServletRequest request) {

        var prop = backend.lookup().entityFor(annotationPropertyId, ont, OWLAnnotationProperty.class);

        var relationsHierarchyService = commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        var primaryHierarchy = propertiesService.getHierarchyService(ont);

        commonRelations.buildPrimaryTree(prop, primaryHierarchy, "Annotations on", model);
        commonRelations.buildSecondaryTree(relationsHierarchyService, null, model, request);

        model.addAttribute("stats", backend.getStats().getAnnotationPropertyStats(statsName, ont, primaryHierarchy));
        model.addAttribute("statsName", statsName);

        commonRelations.renderEntity(prop, model);

        return new ModelAndView(RELATION_TEMPLATE);
    }

    @GetMapping(value = "/{annotationPropertyId}/secondary")
    public ModelAndView getSecondaryFragment(
            @PathVariable final String annotationPropertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            final Model model,
            HttpServletRequest request
    ) {

        var prop = backend.lookup().entityFor(annotationPropertyId, ont, OWLAnnotationProperty.class);

        var relationsHierarchyService = commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        commonRelations.buildSecondaryTree(relationsHierarchyService, null, model, request);

        return new ModelAndView("tree::secondaryhierarchy");
    }

    @GetMapping(value = "/{annotationPropertyId}/withindividual/{individualId}")
    public ModelAndView getRelationsForAnnotationProperty(
            @PathVariable final String annotationPropertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @ModelAttribute final OWLOntology ont,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @RequestParam(defaultValue = "annotationsCount") final String statsName,
            final Model model,
            HttpServletRequest request
    ) {

        var individual = backend.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        commonFragments.getOWLIndividualFragment(individualsService, individual, false,
                with, ont, model, request.getQueryString());

        var prop = backend.lookup().entityFor(annotationPropertyId, ont, OWLAnnotationProperty.class);

        var relationsHierarchyService = commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        var primaryHierarchy = propertiesService.getHierarchyService(ont);

        commonRelations.buildPrimaryTree(prop, primaryHierarchy, "Annotations on", model);
        commonRelations.buildSecondaryTree(relationsHierarchyService, individual, model, request);

        return new ModelAndView(RELATION_TEMPLATE);
    }

    @GetMapping(value = "/{annotationPropertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String annotationPropertyId,
            @RequestParam(defaultValue = "relationsCount") final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {

        var prop = backend.lookup().entityFor(annotationPropertyId, ont, OWLAnnotationProperty.class);

        var stats = backend.getStats().getAnnotationPropertyStats(statsName, ont, propertiesService.getHierarchyService(ont));

        model.addAttribute("t", propertiesService.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("stats", stats);
        model.addAttribute("statsName", statsName);

        URLScheme urlScheme = new CommonRelationsURLScheme<>("/relations/" + PATH, prop, backend.getIriShortFormProvider())
                .withQuery(request.getQueryString());

        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            model.addAttribute("mos", mos.withURLScheme(urlScheme).withActiveObject(prop));
        }

        return new ModelAndView(CommonRelations.BASE_TREE);
    }

    @GetMapping(value = "/{annotationPropertyId}/withindividual/{individualId}/children")
    public ModelAndView getChildren(
            @PathVariable final String annotationPropertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            HttpServletRequest request
    ) {

        var prop = backend.lookup().entityFor(annotationPropertyId, ont, OWLAnnotationProperty.class);
        OWLNamedIndividual individual = commonRelations.getOWLIndividualFor(individualId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>("/relations/" + PATH, prop, backend.getIriShortFormProvider())
                .withTree(relationsHierarchyService)
                .withQuery(request.getQueryString());

        var stats = backend.getStats().getTreeStats(commonRelations.createMemo(relationsHierarchyService), relationsHierarchyService);

        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            model.addAttribute("mos", mos.withURLScheme(urlScheme));
        }

        model.addAttribute("t", relationsHierarchyService.getChildren(individual));
        model.addAttribute("stats", stats);
        model.addAttribute("statsName", "treeStats"); // TODO not used

        return new ModelAndView(CommonRelations.BASE_TREE);
    }
}
