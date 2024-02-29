package org.coode.www.controller;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ProjectInfo;
import org.coode.www.model.paging.With;
import org.coode.www.renderer.RendererFactory;
import org.coode.www.url.CommonRelationsURLScheme;
import org.coode.www.url.RelationPropertyURLScheme;
import org.coode.www.url.URLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.coode.www.service.hierarchy.Relation;
import org.coode.www.url.CommonRelationsURLScheme;
import org.coode.www.url.RelationPropertyURLScheme;
import org.coode.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Controller
@RequestMapping(value = "/relations/" + OWLPropertyRelationsController.PATH)
public class OWLPropertyRelationsController extends ApplicationController {

    public static final String PATH = "onproperty";
    public static final String RELATION_TEMPLATE = "relation";

    private final OWLObjectPropertiesService propertiesService;

    private final ReasonerFactoryService reasonerFactoryService;

    private final CommonRelations<OWLObjectProperty> common;

    public OWLPropertyRelationsController(
            @Autowired OWLObjectPropertiesService propertiesService,
            @Autowired OWLIndividualsService individualsService,
            @Autowired ReasonerFactoryService reasonerFactoryService,
            @Autowired OWLHTMLKit kit,
            @Autowired RendererFactory rendererFactory,
            @Autowired ProjectInfo projectInfo) {

        this.propertiesService = propertiesService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.rendererFactory = rendererFactory;
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
    public String getRelationsForProperty() {
        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        String id = propertiesService.getIdFor(owlTopObjectProperty);
        return "redirect:/relations/" + PATH + "/" + id + "/";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public String getRelationsForProperty(
        @PathVariable final String propertyId,
        @RequestParam(defaultValue = "false") final boolean inverse,
        @RequestParam final @Nullable String orderBy,
        final Model model,
        HttpServletRequest request
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, null, ont, model, request);

        common.renderEntity(property, model);

        return RELATION_TEMPLATE;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public String getRelationsForProperty(
        @PathVariable final String propertyId,
        @PathVariable final String individualId,
        @RequestParam(defaultValue = "false") final boolean inverse,
        @RequestParam final @Nullable String orderBy,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false) List<With> with,
        final Model model,
        HttpServletRequest request
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        OWLNamedIndividual individual = common.renderIndividual(individualId, ont, withOrEmpty, pageSize, request, model, kit.getComparator());

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, individual, ont, model, request);

        return RELATION_TEMPLATE;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}/fragment")
    public String getRelationsForPropertyFragment(
            @PathVariable final String propertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            HttpServletRequest request
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        OWLNamedIndividual individual = common.renderIndividual(individualId, ont, withOrEmpty, pageSize, request, model, kit.getComparator());

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        // TODO should do common without creating the tree
        common.buildCommon(relationsHierarchyService, individual, ont, model, request);

        return "owlentityfragment";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/children")
    public String getChildren(
        @PathVariable final String propertyId,
        final Model model
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, ont);

        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                Comparator.comparing(o -> o.value.iterator().next()));

        model.addAttribute("t", hierarchyService.getChildren(property));
        model.addAttribute("mos", rendererFactory.getRenderer(ont).withURLScheme(new RelationPropertyURLScheme()));

        return CommonRelations.BASE_TREE;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}/children")
    public String getChildren(
        @PathVariable final String propertyId,
        @PathVariable final String individualId,
        @RequestParam(defaultValue = "false") final boolean inverse,
        @RequestParam final @Nullable String orderBy,
        final Model model,
        HttpServletRequest request
    ) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();
        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, ont);
        OWLNamedIndividual individual = common.getOWLIndividualFor(individualId, ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>(relationsHierarchyService,
                "/relations/" + PATH, property).withQuery(request.getQueryString());

        model.addAttribute("t", relationsHierarchyService.getChildren(new Relation<>(property, individual)));
        model.addAttribute("mos", rendererFactory.getRenderer(ont).withURLScheme(urlScheme));

        return CommonRelations.BASE_TREE;
    }
}
