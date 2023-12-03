package org.coode.www.controller;

import org.coode.html.url.CommonRelationsURLScheme;
import org.coode.html.url.RelationPropertyURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ProjectInfo;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;

@Controller
@RequestMapping(value = "/relations/" + OWLPropertyRelationsController.PATH)
public class OWLPropertyRelationsController {

    public static final String PATH = "onproperty";
    public static final String OWLENTITY = "owlentity";
    public static final String BASE_TREE = "base :: tree";

    private final OWLObjectPropertiesService propertiesService;

    private final OWLIndividualsService individualsService;

    private final ReasonerFactoryService reasonerFactoryService;

    private final OWLHTMLKit kit;

    private final ProjectInfo projectInfo;

    private final CommonRelations<OWLObjectProperty> common;

    @ModelAttribute("projectInfo")
    public ProjectInfo getProjectInfo() {
        return projectInfo;
    }

    @ModelAttribute("kit")
    public OWLHTMLKit getKit() {
        return kit;
    }

    public OWLPropertyRelationsController(
            @Autowired OWLObjectPropertiesService propertiesService,
            @Autowired OWLIndividualsService individualsService,
            @Autowired ReasonerFactoryService reasonerFactoryService,
            @Autowired OWLHTMLKit kit,
            @Autowired ProjectInfo projectInfo) {
        this.propertiesService = propertiesService;
        this.individualsService = individualsService;
        this.reasonerFactoryService = reasonerFactoryService;
        this.kit = kit;
        this.projectInfo = projectInfo;
        this.common = new CommonRelations<>(
                PATH,
                kit,
                propertiesService,
                individualsService
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
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model,
                                          HttpServletRequest request) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, kit);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, null, model, request);

        common.renderEntity(property, model);

        return OWLENTITY;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @PathVariable final String individualId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model,
                                          HttpServletRequest request) throws NotFoundException {

        OWLNamedIndividual individual = common.renderIndividual(individualId, model);

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, kit);

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, individual, model, request);

        return OWLENTITY;
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/children")
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, kit);

        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(kit.getActiveOntology()),
                Comparator.comparing(o -> o.value.iterator().next()));

        model.addAttribute("t", hierarchyService.getChildren(property));
        model.addAttribute("mos", new OWLHTMLRenderer(kit).withURLScheme(new RelationPropertyURLScheme()));

        return BASE_TREE;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}/children")
    public String getChildren(@PathVariable final String propertyId,
                              @PathVariable final String individualId,
                              @RequestParam(defaultValue = "false") final boolean inverse,
                              @RequestParam final @Nullable String orderBy,
                              final Model model,
                              HttpServletRequest request) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, kit);
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, kit.getOntologies());

        AbstractRelationsHierarchyService<OWLObjectProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, orderBy, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>(relationsHierarchyService,
                "/relations/" + PATH, property).withQuery(request.getQueryString());

        model.addAttribute("t", relationsHierarchyService.getChildren(individual));
        model.addAttribute("mos", new OWLHTMLRenderer(kit).withURLScheme(urlScheme));

        return BASE_TREE;
    }
}
