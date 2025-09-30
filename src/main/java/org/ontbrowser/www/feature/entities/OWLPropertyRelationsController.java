package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLObjectPropertyHierarchyService;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.List;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE_STR;
import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/relations/" + OWLPropertyRelationsController.PATH)
public class OWLPropertyRelationsController {

    public static final String PATH = "onproperty";
    public static final String RELATION_TEMPLATE = "relation";
    private final OWLObjectPropertiesService propertiesService;
    private final OWLIndividualsService individualsService;
    private final ReasonerFactoryService reasonerFactoryService;
    private final OWLHTMLKit kit;
    private final StatsService statsService;
    private final CommonRelations<OWLObjectProperty> common;
    private final CommonContent commonContent;
    private final CommonFragments commonFragments;

    public OWLPropertyRelationsController(
            OWLObjectPropertiesService propertiesService,
            OWLIndividualsService individualsService,
            ReasonerFactoryService reasonerFactoryService,
            OWLHTMLKit kit,
            StatsService statsService,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        this.propertiesService = propertiesService;
        this.individualsService = individualsService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.kit = kit;
        this.statsService = statsService;
        this.commonContent = commonContent;

        this.common = new CommonRelations<>(
                PATH,
                kit,
                propertiesService,
                statsService
        );

        this.commonFragments = commonFragments;
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

        commonContent.addCommonContent(request.getQueryString(), model, ont);

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

        return new ModelAndView("tree::secondaryhierarchy");
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public ModelAndView getRelationsForProperty(
            @PathVariable final String propertyId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inverse,
            @RequestParam final @Nullable String orderBy,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @RequestParam(defaultValue = "relationsCount") final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            HttpServletRequest request
    ) {
        var individual = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        String queryString = request.getQueryString();

        commonFragments.getOWLIndividualFragment(individualsService, individual,
                false, with, ont, model, queryString);

        var property = kit.lookup().entityFor(propertyId, ont, OWLObjectProperty.class);

        commonContent.addCommonContent(queryString, model, ont);

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
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            HttpServletRequest request
    ) {
        var individual = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        commonFragments.getOWLIndividualFragment(individualsService, individual, false, with,
                ont, model, request.getQueryString());

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

        var mos = (OWLHTMLRenderer)model.getAttribute("mos");
        if (mos != null) {
            model.addAttribute("mos", mos.withURLScheme(new RelationPropertyURLScheme()));
        }

        model.addAttribute("t", hierarchyService.getChildren(property));
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

        var mos = (OWLHTMLRenderer)model.getAttribute("mos");
        if (mos != null) {
            model.addAttribute("mos", mos.withURLScheme(urlScheme));
        }

        model.addAttribute("t", relationsHierarchyService.getChildren(individual));
        model.addAttribute("stats", statsService.getTreeStats(common.createMemo(relationsHierarchyService), relationsHierarchyService));
        model.addAttribute("statsName", "treeStats"); // TODO not used

        return new ModelAndView(CommonRelations.BASE_TREE);
    }

}
