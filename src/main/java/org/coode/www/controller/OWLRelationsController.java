package org.coode.www.controller;

import org.coode.html.url.*;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.*;
import org.coode.www.service.hierarchy.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;

@Controller
@RequestMapping(value = "/relations")
public class OWLRelationsController extends ApplicationController {

    @Autowired
    private OWLObjectPropertiesService propertiesService;

    @Autowired
    private OWLAnnotationPropertiesService annotationPropertiesService;

    @Autowired
    private OWLIndividualsService individualsService;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @GetMapping(value = "/")
    public String getRelations() {
        return "redirect:/relations/onproperty/";
    }

    @GetMapping(value = "/onproperty/")
    public String getRelationsForProperty() {
        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();
        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        String id = propertiesService.getIdFor(owlTopObjectProperty);
        return "redirect:/relations/onproperty/" + id;
    }

    @GetMapping(value = "/onannotationproperty/")
    public String getRelationsForAnnotationProperty() throws NotFoundException {
        Set<OWLAnnotationProperty> props = kit.getActiveOntology().getAnnotationPropertiesInSignature(Imports.INCLUDED);
        if (props.isEmpty()) {
            throw new NotFoundException("Annotation Property");
        }
        // Start with random
        String id = annotationPropertiesService.getIdFor(props.iterator().next());
        return "redirect:/relations/onannotationproperty/" + id;
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/onproperty/{propertyId}")
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model,
                                          HttpServletRequest request) throws NotFoundException {
        PropertiesService<OWLObjectProperty> service = propertiesService;
        OWLObjectProperty property = service.getPropertyFor(propertyId, kit);

        renderEntity(property, model);

        AbstractOWLHierarchyService<OWLNamedIndividual> relationsHierarchyService = buildCommon(
                "onproperty", property, null, service, orderBy, inverse,
                model, request);

        model.addAttribute("hierarchy2", relationsHierarchyService.getTree());

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/onannotationproperty/{propertyId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {
        PropertiesService<OWLAnnotationProperty> service = annotationPropertiesService;
        OWLAnnotationProperty property = service.getPropertyFor(propertyId, kit);

        renderEntity(property, model);

        AbstractOWLHierarchyService<OWLNamedIndividual> relationsHierarchyService = buildCommon(
                "onannotationproperty", property, null, service, orderBy, inverse,
                model, request);

        model.addAttribute("hierarchy2", relationsHierarchyService.getTree());

        return "owlentity";
    }

    @GetMapping(value = "/onannotationproperty/{propertyId}/withindividual/{individualId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @PathVariable final String individualId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {
        PropertiesService<OWLAnnotationProperty> service = annotationPropertiesService;
        OWLAnnotationProperty property = service.getPropertyFor(propertyId, kit);

        OWLNamedIndividual individual = renderIndividual(individualId, model);

        AbstractOWLHierarchyService<OWLNamedIndividual> relationsHierarchyService = buildCommon(
                "onannotationproperty", property, individual, service, orderBy, inverse, model, request);

        model.addAttribute("hierarchy2", relationsHierarchyService.getPrunedTree(individual));

        return "owlentity";
    }

    @GetMapping(value = "/onproperty/{propertyId}/withindividual/{individualId}")
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @PathVariable final String individualId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model,
                                          HttpServletRequest request) throws NotFoundException {
        PropertiesService<OWLObjectProperty> service = propertiesService;
        OWLObjectProperty property = service.getPropertyFor(propertyId, kit);

        OWLNamedIndividual individual = renderIndividual(individualId, model);

        AbstractOWLHierarchyService<OWLNamedIndividual> relationsHierarchyService = buildCommon(
                "onproperty", property, individual, service, orderBy, inverse,
                model, request);

        model.addAttribute("hierarchy2", relationsHierarchyService.getPrunedTree(individual));

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/onproperty/{propertyId}/children")
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, kit);

        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(kit.getActiveOntology()),
                Comparator.comparing(o -> o.value.iterator().next()));

        model.addAttribute("t", hierarchyService.getChildren(property));
        model.addAttribute("mos", new OWLHTMLRenderer(kit).withURLScheme(new RelationPropertyURLScheme(kit)));

        return "base :: tree";
    }

    @GetMapping(value = "/onproperty/{propertyId}/withindividual/{individualId}/children")
    public String getChildren(@PathVariable final String propertyId,
                              @PathVariable final String individualId,
                              @RequestParam(defaultValue = "false") final boolean inverse,
                              @RequestParam final @Nullable String orderBy,
                              final Model model,
                              HttpServletRequest request) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getPropertyFor(propertyId, kit);
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, kit.getOntologies());

        OWLOntology ont = kit.getActiveOntology();

        Comparator<Tree<OWLNamedIndividual>> comparator = (orderBy == null) ?
                Comparator.comparing(o -> o.value.iterator().next()) :
                new PropComparator(propertiesService.getPropertyFor(orderBy, kit), ont);

        AbstractRelationsHierarchyService<OWLObjectProperty> relHierarchy =
                new RelationsHierarchyService(comparator).withProperties(property, ont, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>(kit, relHierarchy,
                "/relations/onproperty", property).withQuery(request.getQueryString());

        model.addAttribute("t", relHierarchy.getChildren(individual));
        model.addAttribute("mos", new OWLHTMLRenderer(kit).withURLScheme(urlScheme));

        return "base :: tree";
    }

    private void renderEntity(OWLEntity entity, Model model) {
        String shortForm = kit.getShortFormProvider().getShortForm(entity);
        String type = entity.getEntityType().getPrintName();
        model.addAttribute("title", shortForm + " (" + type + ")");
        model.addAttribute("iri", entity.getIRI());
    }

    private OWLNamedIndividual renderIndividual(@PathVariable String individualId, Model model) throws NotFoundException {
        Set<OWLOntology> ontologies = kit.getOntologies();
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ontologies);
        renderEntity(individual, model);
        model.addAttribute("characteristics", individualsService.getCharacteristics(individual, ontologies, kit.getComparator()));
        return individual;
    }

    private <T extends OWLProperty> AbstractRelationsHierarchyService<T> buildCommon(
            String path,
            T property,
            @Nullable OWLNamedIndividual individual,
            PropertiesService<T> propertyService,
            @Nullable String orderBy,
            boolean inverse,
            Model model,
            HttpServletRequest request) throws NotFoundException {

        OWLOntology ont = kit.getActiveOntology();

        T orderByProperty = (orderBy != null) ? propertyService.getPropertyFor(orderBy, kit) : null;

        Comparator<Tree<OWLNamedIndividual>> comparator = propertyService.getComparator(orderByProperty, ont);

        // relations tree
        AbstractRelationsHierarchyService<T> relationsHierarchyService = propertyService
                .getRelationsHierarchy(comparator)
                .withProperties(property, ont, inverse);

        URLScheme urlScheme = new CommonRelationsURLScheme<>(kit, relationsHierarchyService,
                "/relations/" + path, property).withQuery(request.getQueryString());

        model.addAttribute("type", "Relations on");
        model.addAttribute("hierarchy", propertyService.getPropTree(property, ont));
        model.addAttribute("type2", kit.getShortFormProvider().getShortForm(property));
        model.addAttribute("inverse", inverse);

        Set<OWLObject> activeObjects = new HashSet<>();
        activeObjects.add(property);
        if (individual != null) {
            activeObjects.add(individual);
        }

        model.addAttribute("mos", new OWLHTMLRenderer(kit)
                .withActiveObjects(activeObjects)
                .withURLScheme(urlScheme));

        return relationsHierarchyService;
    }
}
