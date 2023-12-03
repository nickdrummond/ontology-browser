package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ProjectInfo;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
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
public class OWLAnnotationRelationsController {

    public static final String PATH = "onannotationproperty";
    public static final String OWLENTITY = "owlentity";

    private final OWLAnnotationPropertiesService propertiesService;

    private final OWLHTMLKit kit;

    private final ProjectInfo projectInfo;

    private final CommonRelations<OWLAnnotationProperty> common;

    @ModelAttribute("projectInfo")
    public ProjectInfo getProjectInfo() {
        return projectInfo;
    }

    @ModelAttribute("kit")
    public OWLHTMLKit getKit() {
        return kit;
    }

    public OWLAnnotationRelationsController(
            @Autowired OWLAnnotationPropertiesService propertiesService,
            @Autowired OWLIndividualsService individualsService,
            @Autowired OWLHTMLKit kit,
            @Autowired ProjectInfo projectInfo) {
        this.propertiesService = propertiesService;
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

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, kit);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, null, model, request);

        common.renderEntity(property, model);

        return OWLENTITY;
    }

    @GetMapping(value = "/{propertyId}/withindividual/{individualId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @PathVariable final String individualId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {

        OWLNamedIndividual individual = common.renderIndividual(individualId, model);

        OWLAnnotationProperty property = propertiesService.getPropertyFor(propertyId, kit);

        AbstractRelationsHierarchyService<OWLAnnotationProperty> relationsHierarchyService =
                common.getRelationsHierarchyService(property, orderBy, inverse);

        common.buildCommon(relationsHierarchyService, individual, model, request);

        return OWLENTITY;
    }
}
