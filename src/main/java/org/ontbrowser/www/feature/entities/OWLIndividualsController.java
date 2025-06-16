package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.ClassCharacteristicsBuilder;
import org.ontbrowser.www.feature.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.feature.stats.Stats;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
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
public class OWLIndividualsController extends ApplicationController {

    private static final int DEFAULT_SECONDARY_PAGE_SIZE = 60;

    private final OWLIndividualsService individualsService;
    private final OWLClassesService owlClassesService;
    private final ReasonerFactoryService reasonerFactoryService;
    private final ReasonerService reasonerService;
    private final StatsService statsService;
    private final CommonContent commonContent;

    public OWLIndividualsController(
            OWLIndividualsService individualsService,
            OWLClassesService owlClassesService,
            ReasonerFactoryService reasonerFactoryService,
            ReasonerService reasonerService,
            StatsService statsService, CommonContent commonContent) {
        this.individualsService = individualsService;
        this.owlClassesService = owlClassesService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.reasonerService = reasonerService;
        this.statsService = statsService;
        this.commonContent = commonContent;
    }

    private CommonFragments getCommon() {
        return new CommonFragments(kit, projectInfo, reasonerService);
    }


    @GetMapping(value = "/")
    public void getOWLIndividualsOld(
            final HttpServletResponse response
    ) throws IOException {
        getOWLIndividuals(response);
    }

    @GetMapping()
    public void getOWLIndividuals(
            final HttpServletResponse response
    ) throws IOException {
        response.sendRedirect("/individuals/by/type");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{individualId}")
    public ModelAndView getOWLIndividual(
            @PathVariable final String individualId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            @RequestParam(defaultValue = "false") final boolean inferred,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response
    ) {
        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var owlIndividual = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);
        OWLClass firstType = individualsService.getNamedTypes(owlIndividual, ont).stream().findFirst()
                .orElse(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing());

        return byType(
                kit.lookup().getId(firstType),
                individualId, inferred,
                "inferredInstances", withOrEmpty, ont, model, request, response);
    }

    @GetMapping(value = "/by/type")
    public void byType(
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response
    ) throws IOException {
        response.sendRedirect("/individuals/by/type/" +
                kit.lookup().getId(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing()));
    }

    @GetMapping(value = "/by/type/{classId}")
    public ModelAndView byType(
            @PathVariable final String classId,
            @RequestParam(defaultValue = "inferredInstances") final String statsName,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response
    ) {
        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        commonContent.addCommonContent(request, model, ont);

        OWLClass type = buildNav(classId, withOrEmpty, statsName, ont, model, request);

        getOWLClassFragment(classId, ont, withOrEmpty, model, request, response);

        model.addAttribute("mos", getRenderer(type, null, ont, request));

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

        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, with));

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
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response
    ) {
        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var ind = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        commonContent.addCommonContent(request, model, ont);

        OWLClass type = buildNav(classId, withOrEmpty, statsName, ont, model, request);

        getOWLIndividualFragment(individualId, inferred, withOrEmpty, ont, model, request, response);

        model.addAttribute("mos", getRenderer(type, ind, ont, request));

        return new ModelAndView("instances");
    }

    private OWLHTMLRenderer getRenderer(
            OWLClass type, OWLNamedIndividual ind,
            OWLOntology ont, HttpServletRequest request) {

        URLScheme urlScheme = new CommonRelationsURLScheme<>("/individuals/by/type", type)
                .withQuery(request.getQueryString());

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(type);
        if (ind != null) {
            activeObjects.add(ind);
        }
        return rendererFactory.getHTMLRenderer(ont).withActiveObjects(activeObjects).withURLScheme(urlScheme);
    }

    private OWLHTMLRenderer getRenderer(OWLClass type, OWLOntology ont, HttpServletRequest request) {
        URLScheme urlScheme = new CommonRelationsURLScheme<>("/individuals/by/type", type)
                .withQuery(request.getQueryString());

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(type);
        return rendererFactory.getHTMLRenderer(ont).withActiveObjects(activeObjects).withURLScheme(urlScheme);
    }

    private void buildPrimaryHierarchy(String statsName, Model model, OWLClass type, OWLReasoner r) {
        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, treeComparator());
        Tree<OWLClass> prunedTree = hierarchyService.getPrunedTree(type);

        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("stats", statsService.getClassStats(statsName, r));
        model.addAttribute("statsName", statsName);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/by/type/{classId}/children")
    public ModelAndView getChildren(
            @PathVariable final String classId,
            @RequestParam final String statsName,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request) {

        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);
        OWLHTMLRenderer owlRenderer = getRenderer(type, ont, request);
        Stats stats = statsService.getClassStats(statsName, r);

        return getCommon().getClassChildren(type, r, owlRenderer, stats, model);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{individualId}/fragment")
    public ModelAndView getOWLIndividualFragment(
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "false") final boolean inferred,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var ind = kit.lookup().entityFor(individualId, ont, OWLNamedIndividual.class);

        // TODO custom renderer - linked to tree (see relations)
        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(ind);
        return getCommon().getOWLIndividualFragment(individualsService, ind, inferred, withOrEmpty, ont, owlRenderer, model, request, response);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/by/type/{classId}/fragment")
    public ModelAndView getOWLClassFragment(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {
        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);

        // TODO custom renderer
        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(type);
        return getCommon().getOWLClassFragment(owlClassesService, type, ont, owlRenderer, withOrEmpty, model, request, response);
    }

    @GetMapping(value = "/by/type/{classId}/secondary")
    public ModelAndView getSecondaryFragment(
            @PathVariable final String classId,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        var type = kit.lookup().entityFor(classId, ont, OWLClass.class);

        buildSecondaryNavigation(ont, withOrEmpty, model, type);

        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, with));
        model.addAttribute("mos", getRenderer(type, null, ont, request));

        return new ModelAndView("base::secondary");
    }

    private void buildSecondaryNavigation(OWLOntology ont, List<With> with, Model model, OWLClass type) {
        var characteristic = owlClassesService.getCharacteristic(
                ClassCharacteristicsBuilder.MEMBERS,
                type, ont, kit.getComparator(), with, DEFAULT_SECONDARY_PAGE_SIZE);

        model.addAttribute("direct",
                characteristic.orElse(new Characteristic(type, ClassCharacteristicsBuilder.MEMBERS, Collections.emptyList())));
    }
}
