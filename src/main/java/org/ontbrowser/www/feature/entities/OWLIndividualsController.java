package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.ClassCharacteristicsBuilder;
import org.ontbrowser.www.feature.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/individuals")
public class OWLIndividualsController {

    private static final int DEFAULT_SECONDARY_PAGE_SIZE = 60;

    private final OWLHTMLKit kit;
    private final OWLIndividualsService individualsService;
    private final OWLClassesService owlClassesService;
    private final ReasonerFactoryService reasonerFactoryService;
    private final StatsService statsService;
    private final CommonContent commonContent;
    private final CommonFragments commonFragments;

    public OWLIndividualsController(
            OWLHTMLKit kit,
            OWLIndividualsService individualsService,
            OWLClassesService owlClassesService,
            ReasonerFactoryService reasonerFactoryService,
            StatsService statsService,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        this.kit = kit;
        this.individualsService = individualsService;
        this.owlClassesService = owlClassesService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.statsService = statsService;
        this.commonContent = commonContent;
        this.commonFragments = commonFragments;
    }

    @GetMapping(value = "/")
    public void getOWLIndividualsOld(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {
        getOWLIndividuals(ontId, response);
    }

    @GetMapping()
    public void getOWLIndividuals(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {
        response.sendRedirect("/individuals/by/type" + (ontId != null ? "?ontId=" + ontId : ""));
    }

    @GetMapping(value = "/{individualId}")
    public ModelAndView getOWLIndividual(
            @PathVariable final String individualId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @RequestParam(defaultValue = "false") final boolean inferred,
            final Model model,
            final HttpServletRequest request
    ) {
        var owlIndividual = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);
        OWLEntity firstType = individualsService.getNamedTypes(owlIndividual, ont).stream().findFirst()
                .orElse(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing());

        return byType(
                kit.lookup().getId(firstType),
                individualId, inferred,
                "inferredInstances", with, ont, model, request);
    }

    @GetMapping(value = "/by/type")
    public void byType(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {
        String thingId = kit.lookup().getId(kit.getOWLOntologyManager().getOWLDataFactory().getOWLThing());
        response.sendRedirect("/individuals/by/type/" + thingId + (ontId != null ? "?ontId=" + ontId : ""));
    }

    @GetMapping(value = "/by/type/{classId}")
    public ModelAndView byType(
            @PathVariable final String classId,
            @RequestParam(defaultValue = "inferredInstances") final String statsName,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        commonContent.addCommonContent(request.getQueryString(), model, ont);

        OWLClass type = buildNav(classId, with, statsName, ont, model, request);

        getOWLClassFragment(classId, ont, with, model, request);

        updateRenderer(type, null, model, request);

        return new ModelAndView("instances");
    }

    private OWLClass buildNav(
            final String classId,
            final List<With> with,
            final String statsName,
            final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request.getQueryString(), with));

        buildPrimaryHierarchy(statsName, model, type, r);

        buildSecondaryNavigation(ont, with, model, type);

        return type;
    }

    @GetMapping(value = "/by/type/{classId}/withindividual/{individualId}")
    public ModelAndView byType(
            @PathVariable final String classId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inferred,
            @RequestParam(defaultValue = "inferredInstances") final String statsName,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var ind = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        commonContent.addCommonContent(request.getQueryString(), model, ont);

        OWLClass type = buildNav(classId, with, statsName, ont, model, request);

        getOWLIndividualFragment(individualId, inferred, with, ont, model, request);

        updateRenderer(type, ind, model, request);

        return new ModelAndView("instances");
    }

    private void updateRenderer(
            OWLClass type, OWLNamedIndividual ind,
            Model model, HttpServletRequest request) {

        var urlScheme = new CommonRelationsURLScheme<>("/individuals/by/type", type)
                .withQuery(request.getQueryString());

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(type);
        if (ind != null) {
            activeObjects.add(ind);
        }

        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            mos.withActiveObjects(activeObjects).withURLScheme(urlScheme);
        }
    }

    private void updateRenderer(OWLClass type, Model model, HttpServletRequest request) {
        URLScheme urlScheme = new CommonRelationsURLScheme<>("/individuals/by/type", type)
                .withQuery(request.getQueryString());

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(type);
        var mos = (OWLHTMLRenderer) model.getAttribute("mos");
        if (mos != null) {
            mos.withActiveObjects(activeObjects).withURLScheme(urlScheme);
        }
    }

    private void buildPrimaryHierarchy(String statsName, Model model, OWLClass type, OWLReasoner r) {
        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, treeComparator());
        Tree<OWLClass> prunedTree = hierarchyService.getPrunedTree(type);

        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("stats", statsService.getClassStats(statsName, r));
        model.addAttribute("statsName", statsName);
    }

    @GetMapping(value = "/by/type/{classId}/children")
    public ModelAndView getChildren(
            @PathVariable final String classId,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request) {

        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);

        var r = reasonerFactoryService.getToldReasoner(ont);
        updateRenderer(type, model, request);
        var stats = statsService.getClassStats(statsName, r);

        return commonFragments.getClassChildren(type, r, stats, model);
    }

    @GetMapping(value = "/{individualId}/fragment")
    public ModelAndView getOWLIndividualFragment(
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inferred,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var ind = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);
        return commonFragments.getOWLIndividualFragment(individualsService, ind, inferred, with,
                ont, model, request.getQueryString());
    }

    @GetMapping(value = "/by/type/{classId}/fragment")
    public ModelAndView getOWLClassFragment(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);
        return commonFragments.getOWLClassFragment(owlClassesService, type, false, ont,
                with, model, request.getQueryString());
    }

    @GetMapping(value = "/by/type/{classId}/secondary")
    public ModelAndView getSecondaryFragment(
            @PathVariable final String classId,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);

        buildSecondaryNavigation(ont, with, model, type);

        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request.getQueryString(), with));

        updateRenderer(type, null, model, request);

        return new ModelAndView("tree::secondary");
    }

    private void buildSecondaryNavigation(OWLOntology ont, List<With> with, Model model, OWLClass type) {
        var characteristic = owlClassesService.getCharacteristicsBuilder(
                type, ont, kit.getComparator(), with, DEFAULT_SECONDARY_PAGE_SIZE)
                .getCharacteristic(ClassCharacteristicsBuilder.MEMBERS);

        model.addAttribute("direct",
                characteristic.orElse(new Characteristic(type, ClassCharacteristicsBuilder.MEMBERS, Collections.emptyList())));
    }
}
