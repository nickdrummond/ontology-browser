package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
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

    private final OWLHTMLKit kit;
    private final OWLAnnotationPropertiesService propertiesService;
    private final OWLIndividualsService individualsService;
    private final StatsService statsService;
    private final CommonRelations<OWLAnnotationProperty> commonRelations;
    private final CommonFragments commonFragments;

    public OWLAnnotationRelationsController(
            OWLHTMLKit kit,
            OWLAnnotationPropertiesService propertiesService,
            OWLIndividualsService individualsService,
            StatsService statsService,
            CommonFragments commonFragments
    ) {
        this.kit = kit;
        this.propertiesService = propertiesService;
        this.individualsService = individualsService;
        this.statsService = statsService;
        this.commonRelations = new CommonRelations<>(
                PATH,
                kit,
                propertiesService,
                statsService
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
        Set<OWLAnnotationProperty> props = kit.getRootOntology().getAnnotationPropertiesInSignature(Imports.INCLUDED);
        if (props.isEmpty()) {
            throw new ResponseStatusException(NOT_FOUND, "Annotation Properties not found");
        }
        // Start with random
        String id = kit.lookup().getId(props.iterator().next());
        response.sendRedirect("/relations/" + PATH + "/" + id);
    }


    @GetMapping(value = "/{propertyId}")
    public ModelAndView getRelationsForAnnotationProperty(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(defaultValue = "annotationsCount") final String statsName,
            final Model model,
            HttpServletRequest request) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        var relationsHierarchyService = commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        var primaryHierarchy = propertiesService.getHierarchyService(ont);

        commonRelations.buildPrimaryTree(prop, primaryHierarchy, "Annotations on", model);
        commonRelations.buildSecondaryTree(relationsHierarchyService, null, model, request);

        model.addAttribute("stats", statsService.getAnnotationPropertyStats(statsName, ont, primaryHierarchy));
        model.addAttribute("statsName", statsName);

        commonRelations.renderEntity(prop, model);

        return new ModelAndView(RELATION_TEMPLATE);
    }

    @GetMapping(value = "/{propertyId}/secondary")
    public ModelAndView getSecondaryFragment(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            final Model model,
            HttpServletRequest request
    ) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        var relationsHierarchyService = commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        commonRelations.buildSecondaryTree(relationsHierarchyService, null, model, request);

        return new ModelAndView("tree::secondaryhierarchy");
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public ModelAndView getRelationsForAnnotationProperty(
            @PathVariable final String propertyId,
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

        var individual = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        commonFragments.getOWLIndividualFragment(individualsService, individual, false,
                with, ont, model, request.getQueryString());

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        var relationsHierarchyService = commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        var primaryHierarchy = propertiesService.getHierarchyService(ont);

        commonRelations.buildPrimaryTree(prop, primaryHierarchy, "Annotations on", model);
        commonRelations.buildSecondaryTree(relationsHierarchyService, individual, model, request);

        return new ModelAndView(RELATION_TEMPLATE);
    }

    @GetMapping(value = "/{propertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            @RequestParam(defaultValue = "relationsCount") final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);

        var stats = statsService.getAnnotationPropertyStats(statsName, ont, propertiesService.getHierarchyService(ont));

        model.addAttribute("t", propertiesService.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("stats", stats);
        model.addAttribute("statsName", statsName);

        URLScheme urlScheme = new CommonRelationsURLScheme<>("/relations/" + PATH, prop)
                .withQuery(request.getQueryString());

        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            model.addAttribute("mos", mos.withURLScheme(urlScheme).withActiveObject(prop));
        }

        return new ModelAndView(CommonRelations.BASE_TREE);
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            HttpServletRequest request
    ) {

        var prop = kit.lookup().entityFor(propertyId, ont, OWLAnnotationProperty.class);
        OWLNamedIndividual individual = commonRelations.getOWLIndividualFor(individualId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                commonRelations.getRelationsHierarchyService(prop, ont, orderBy, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>("/relations/" + PATH, prop)
                .withTree(relationsHierarchyService)
                .withQuery(request.getQueryString());

        var stats = statsService.getTreeStats(commonRelations.createMemo(relationsHierarchyService), relationsHierarchyService);

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
