package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ProjectInfo;
import org.coode.www.renderer.RendererFactory;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Set;

@Controller
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
    public String getRelationsForAnnotationProperty() throws NotFoundException {
        Set<OWLAnnotationProperty> props = kit.getActiveOntology().getAnnotationPropertiesInSignature(Imports.INCLUDED);
        if (props.isEmpty()) {
            throw new NotFoundException("Annotation Property");
        }
        // Start with random
        String id = propertiesService.getIdFor(props.iterator().next());
        return "redirect:/relations/" + PATH + "/" + id + "/";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, null, ont, model, request);

        common.renderEntity(property, model);

        return RELATION_TEMPLATE;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @PathVariable final String individualId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        OWLNamedIndividual individual = common.renderIndividual(individualId, ont, model, kit.getComparator());

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, ont);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, ont, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, individual, ont, model, request);

        return RELATION_TEMPLATE;
    }
}
