package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.renderer.RendererFactory;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.ontbrowser.www.service.stats.StatsService;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.Collections;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/relations/" + OWLPropertyRelationsController.PATH)
public class OWLPropertyRelationsController extends ApplicationController {

    public static final String PATH = "onproperty";
    public static final String RELATION_TEMPLATE = "relation";
    private final OWLObjectPropertiesService propertiesService;
    private final ReasonerFactoryService reasonerFactoryService;
    private final StatsService statsService;
    private final CommonRelations<OWLObjectProperty> common;

    public OWLPropertyRelationsController(
            @Autowired OWLObjectPropertiesService propertiesService,
            @Autowired OWLIndividualsService individualsService,
            @Autowired ReasonerFactoryService reasonerFactoryService,
            @Autowired OWLHTMLKit kit,
            @Autowired RendererFactory rendererFactory,
            @Autowired StatsService statsService,
            @Autowired ProjectInfo projectInfo) {

        this.propertiesService = propertiesService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.statsService = statsService;
        this.rendererFactory = rendererFactory;
        this.projectInfo = projectInfo;
        this.kit = kit;

        this.common = new CommonRelations<>(
                PATH,
                kit,
                propertiesService,
                individualsService,
                rendererFactory,
                statsService
        );
    }


    @GetMapping(value = "/")
    public void getRelationsForPropertyOld(
            final HttpServletResponse response
    ) throws IOException {
        getRelationsForProperty(response);
    }

    @GetMapping()
    public void getRelationsForProperty(
            final HttpServletResponse response
    ) throws IOException {
        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        String id = kit.lookup().getId(owlTopObjectProperty);
        response.sendRedirect("/relations/" + PATH + "/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public ModelAndView getRelationsForProperty(
        @PathVariable final String propertyId,
        @ModelAttribute final OWLOntology ont,
        @RequestParam(defaultValue = "false") final boolean inverse,
        @RequestParam final @Nullable String orderBy,
        @RequestParam(defaultValue = "relationsCount") final String statsName,
        final Model model,
        HttpServletRequest request
    ) {
        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        var relationsHierarchyService = common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildPrimaryTree(property, propertiesService.getHierarchyService(ont), "Relations on", model);
        model.addAttribute("stats", statsService.getPropertyStats(statsName, reasonerFactoryService.getToldReasoner(ont)));
        model.addAttribute("statsName", statsName);

        common.buildSecondaryTree(relationsHierarchyService, null, model, request);

        // TODO full property render?
        common.renderEntity(property, model);

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

        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        var relationsHierarchyService = common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildSecondaryTree(relationsHierarchyService, null, model, request);

        return new ModelAndView("base::secondaryhierarchy");
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public ModelAndView getRelationsForProperty(
        @PathVariable final String propertyId,
        @PathVariable final String individualId,
        @RequestParam(defaultValue = "false") final boolean inverse,
        @RequestParam final @Nullable String orderBy,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false) List<With> with,
        @RequestParam(defaultValue = "relationsCount") final String statsName,
        @ModelAttribute final OWLOntology ont,
        final Model model,
        HttpServletRequest request
    ) {

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var individual = common.renderIndividual(individualId, ont, withOrEmpty, pageSize, request, model, kit.getComparator());

        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        var relationsHierarchyService = common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildPrimaryTree(property, propertiesService.getHierarchyService(ont), "Relations on", model);
        model.addAttribute("stats", statsService.getPropertyStats(statsName, reasonerFactoryService.getToldReasoner(ont)));
        model.addAttribute("statsName", statsName);
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());

        common.buildSecondaryTree(relationsHierarchyService, individual, model, request);

        return new ModelAndView(RELATION_TEMPLATE);
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}/fragment")
    public ModelAndView getRelationsForPropertyFragment(
            @PathVariable final String propertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            HttpServletRequest request
    ) {

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var individual = common.renderIndividual(individualId, ont, withOrEmpty, pageSize, request, model, kit.getComparator());

        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        var relationsHierarchyService = common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildSecondaryTree(relationsHierarchyService, individual, model, request);

        model.addAttribute("ontologiesSfp", kit.getOntologySFP());

        return new ModelAndView("owlentityfragment");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/children")
    public ModelAndView getChildren(
        @PathVariable final String propertyId,
        @RequestParam(defaultValue = "relationsCount") final String statsName,
        @ModelAttribute final OWLOntology ont,
        final Model model
    ) {

        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                treeComparator());

        OWLReasoner reasoner = reasonerFactoryService.getToldReasoner(ont);

        model.addAttribute("t", hierarchyService.getChildren(property));
        model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont).withURLScheme(new RelationPropertyURLScheme()));
        model.addAttribute("stats", statsService.getPropertyStats(statsName, reasoner));
        model.addAttribute("statsName", statsName);

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

        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);
        OWLNamedIndividual individual = common.getOWLIndividualFor(individualId, ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>("/relations/" + PATH, property)
                .withTree(relationsHierarchyService)
                .withQuery(request.getQueryString());

        model.addAttribute("t", relationsHierarchyService.getChildren(individual));
        model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont).withURLScheme(urlScheme));
        model.addAttribute("stats", statsService.getTreeStats(common.createMemo(relationsHierarchyService), relationsHierarchyService));
        model.addAttribute("statsName", "treeStats"); // TODO not used

        return new ModelAndView(CommonRelations.BASE_TREE);
    }

}
