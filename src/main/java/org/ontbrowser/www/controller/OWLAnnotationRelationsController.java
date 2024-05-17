package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.RendererFactory;
import org.ontbrowser.www.service.OWLAnnotationPropertiesService;
import org.ontbrowser.www.service.OWLIndividualsService;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.url.CommonRelationsURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collections;
import java.util.List;
import java.util.Set;

@RestController
@RequestMapping(value = "/relations/" + OWLAnnotationRelationsController.PATH)
public class OWLAnnotationRelationsController extends ApplicationController {

    public static final String PATH = "onannotationproperty";
    public static final String RELATION_TEMPLATE = "relation";
    private final OWLAnnotationPropertiesService propertiesService;
    private final CommonRelations<OWLAnnotationProperty> common;


    public OWLAnnotationRelationsController(
            @Autowired OWLAnnotationPropertiesService propertiesService,
            @Autowired OWLIndividualsService individualsService,
            @Autowired OWLHTMLKit kit,
            @Autowired ProjectInfo projectInfo,
            @Autowired RendererFactory rendererFactory) {
        this.propertiesService = propertiesService;
        this.projectInfo = projectInfo;
        this.kit = kit;
        this.common = new CommonRelations<>(
                PATH,
                kit.getShortFormProvider(),
                propertiesService,
                individualsService,
                rendererFactory
        );
    }

    @GetMapping(value = "/")
    public void getRelationsForAnnotationProperty(
            final HttpServletResponse response
    ) throws IOException, NotFoundException {
        Set<OWLAnnotationProperty> props = kit.getActiveOntology().getAnnotationPropertiesInSignature(Imports.INCLUDED);
        if (props.isEmpty()) {
            throw new NotFoundException("Annotation Property");
        }
        // Start with random
        String id = propertiesService.getIdFor(props.iterator().next());
        response.sendRedirect("/relations/" + PATH + "/" + id + "/");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public ModelAndView getRelationsForAnnotationProperty(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            final Model model,
            HttpServletRequest request) throws NotFoundException {

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, null, ont, model, request);

        common.renderEntity(property, model);

        return new ModelAndView(RELATION_TEMPLATE);
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public ModelAndView getRelationsForAnnotationProperty(
            @PathVariable final String propertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @ModelAttribute final OWLOntology ont,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            HttpServletRequest request) throws NotFoundException {

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        OWLNamedIndividual individual = common.renderIndividual(
                individualId,
                ont,
                withOrEmpty,
                pageSize,
                request,
                model,
                kit.getComparator());

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, individual, ont, model, request);

        return new ModelAndView(RELATION_TEMPLATE);
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
    ) throws NotFoundException {

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, ont);
        OWLNamedIndividual individual = common.getOWLIndividualFor(individualId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>(relationsHierarchyService,
                "/relations/" + PATH, property).withQuery(request.getQueryString());

        model.addAttribute("t", relationsHierarchyService.getChildren(individual));
        model.addAttribute("mos", rendererFactory.getRenderer(ont).withURLScheme(urlScheme));

        return new ModelAndView(CommonRelations.BASE_TREE);
    }
}
