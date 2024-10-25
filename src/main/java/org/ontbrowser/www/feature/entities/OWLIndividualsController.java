package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.dlquery.ReasonerService;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.ClassCharacteristicsBuilder;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.*;
import org.ontbrowser.www.service.hierarchy.IndividualsByTypeHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.service.stats.Stats;
import org.ontbrowser.www.service.stats.StatsService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.*;

@RestController
@RequestMapping(value = "/individuals")
public class OWLIndividualsController extends ApplicationController {

    private static final int DEFAULT_SECONDARY_PAGE_SIZE = 60;

    private final OWLIndividualsService individualsService;
    private final OWLClassesService owlClassesService;
    private final MediaService mediaService;
    private final ReasonerFactoryService reasonerFactoryService;
    private final ReasonerService reasonerService;
    private final StatsService statsService;

    public OWLIndividualsController(
            @Autowired OWLIndividualsService individualsService,
            @Autowired OWLClassesService owlClassesService,
            @Autowired MediaService mediaService,
            @Autowired ReasonerFactoryService reasonerFactoryService,
            @Autowired ReasonerService reasonerService,
            @Autowired StatsService statsService) {
        this.individualsService = individualsService;
        this.owlClassesService = owlClassesService;
        this.mediaService = mediaService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.reasonerService = reasonerService;
        this.statsService = statsService;
    }

    private CommonFragments getCommon() {
        return new CommonFragments(kit, projectInfo, reasonerService);
    }

    @GetMapping(value = "/")
    public void getOWLIndividuals(
            final HttpServletResponse response
    ) throws IOException {
        response.sendRedirect("/individuals/by/type/");
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{individualId}")
    public void getOWLIndividual(
            @PathVariable final String individualId,
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response) throws NotFoundException, IOException {

        OWLNamedIndividual owlIndividual = individualsService.getOWLIndividualFor(individualId, ont);
        OWLClass firstType = individualsService.getNamedTypes(owlIndividual, ont).stream().findFirst()
                .orElse(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing());

        response.sendRedirect(
                "/individuals/by/type/" + owlClassesService.getIdFor(firstType) +
                        "/withindividual/" + individualId);
    }

    @GetMapping(value = "/by/type/")
    public void byType(
            @ModelAttribute final OWLOntology ont,
            final HttpServletResponse response
    ) throws IOException {
        response.sendRedirect("/individuals/by/type/" +
                owlClassesService.getIdFor(ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing()));
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
    ) throws NotFoundException {

        OWLClass type = buildNav(classId, statsName, with, ont, model, request);

        getOWLClassFragment(classId, ont, with, model, request, response);

        model.addAttribute("mos", getRenderer(type, null, ont, request));

        return new ModelAndView("instances");
    }

    private OWLClass buildNav(
            String classId,
            String statsName,
            List<With> with,
            OWLOntology ont,
            Model model,
            HttpServletRequest request
    ) throws NotFoundException {
        OWLClass type = owlClassesService.getOWLClassFor(classId, ont);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        buildPrimaryHierarchy(statsName, model, type, r);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        buildSecondaryNavigation(ont, model, type, withOrEmpty, request);

        return type;
    }


    @GetMapping(value = "/by/type/{classId}/withindividual/{individualId}")
    public ModelAndView byType(
            @PathVariable final String classId,
            @PathVariable final String individualId,
            @RequestParam(defaultValue = "inferredInstances") final String statsName,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response
    ) throws NotFoundException {

        OWLNamedIndividual ind = individualsService.getOWLIndividualFor(individualId, ont);

        OWLClass type = buildNav(classId, statsName, with, ont, model, request);

        getOWLIndividualFragment(individualId, false, with, ont, model, request, response);

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
        return rendererFactory.getRenderer(ont).withActiveObjects(activeObjects).withURLScheme(urlScheme);
    }

    private OWLHTMLRenderer getRenderer(OWLClass type, OWLOntology ont, HttpServletRequest request) {
        URLScheme urlScheme = new CommonRelationsURLScheme<>("/individuals/by/type", type)
                .withQuery(request.getQueryString());

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(type);
        return rendererFactory.getRenderer(ont).withActiveObjects(activeObjects).withURLScheme(urlScheme);
    }

    private IndividualsByTypeHierarchyService buildSecondaryHierarchy(Model model, OWLClass type, OWLReasoner r) {
        IndividualsByTypeHierarchyService individualsHierarchy =
                new IndividualsByTypeHierarchyService(Comparator.comparing(o -> o.value.iterator().next()))
                        .withType(type, r);

        model.addAttribute("hierarchy2", individualsHierarchy.getClosedTree());
        model.addAttribute("type2", kit.render(type));

        return individualsHierarchy;
    }

    private void buildPrimaryHierarchy(String statsName, Model model, OWLClass type, OWLReasoner r) throws NotFoundException {
        Comparator<Tree<OWLClass>> comparator = Comparator.comparing(o -> o.value.iterator().next());
        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, comparator);
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
            final HttpServletRequest request) throws NotFoundException {

        OWLClass owlClass = owlClassesService.getOWLClassFor(classId, ont);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);
        OWLHTMLRenderer owlRenderer = getRenderer(owlClass, ont, request);
        Stats stats = statsService.getClassStats(statsName, r);

        return getCommon().getClassChildren(owlClass, r, owlRenderer, stats, model);
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
            final HttpServletResponse response) throws NotFoundException {
        OWLNamedIndividual owlIndividual = individualsService.getOWLIndividualFor(individualId, ont);

        // TODO custom renderer - linked to tree (see relations)
        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlIndividual);
        return getCommon().getOWLIndividualFragment(individualsService, owlIndividual, inferred, with, ont, owlRenderer, model, request, response);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/by/type/{classId}/fragment")
    public ModelAndView getOWLClassFragment(
            @PathVariable final String classId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {
        OWLClass owlClass = owlClassesService.getOWLClassFor(classId, ont);

        // TODO custom renderer
        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlClass);
        return getCommon().getOWLClassFragment(owlClassesService, owlClass, ont, owlRenderer, with, model, request, response);
    }

    @GetMapping(value = "/by/type/{classId}/secondary")
    public ModelAndView getSecondaryFragment(
            @PathVariable final String classId,
            @RequestParam(required = false) List<With> with,
            @ModelAttribute final OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) throws NotFoundException {

        OWLClass type = owlClassesService.getOWLClassFor(classId, ont);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        buildSecondaryNavigation(ont, model, type, withOrEmpty, request);

        model.addAttribute("mos", getRenderer(type, null, ont, request));

        return new ModelAndView("base::secondary");
    }

    private void buildSecondaryNavigation(OWLOntology ont, Model model, OWLClass type,
                                          List<With> withOrEmpty, HttpServletRequest request) {
        var characteristic = owlClassesService.getCharacteristic(
                        ClassCharacteristicsBuilder.MEMBERS,
                        type, ont, kit.getComparator(), withOrEmpty, DEFAULT_SECONDARY_PAGE_SIZE);

        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        model.addAttribute("direct",
                characteristic.orElse(new Characteristic(type, "Members", Collections.emptyList())));
    }
}
