package org.coode.www.controller;

import com.google.common.collect.ImmutableSet;
import org.coode.html.url.AnnotationRelationsURLScheme;
import org.coode.html.url.RelationPropertyURLScheme;
import org.coode.html.url.RelationsURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLAnnotationPropertiesService;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

// TODO remove massive duplication

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

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);

        OWLOntology ont = kit.getActiveOntology();
        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        // property tree
        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(r,
                Comparator.comparing(o -> o.value.iterator().next()));
        Tree<OWLObjectPropertyExpression> propertyTree = hierarchyService.getPrunedTree(property);

        Comparator<Tree<OWLNamedIndividual>> comparator = (orderBy == null) ?
                Comparator.comparing(o -> o.value.iterator().next()) :
                new PropComparator(propertiesService.getOWLObjectPropertyFor(orderBy, kit), ont);

        // relations tree
        RelationsHierarchyService relationsHierarchyService = new RelationsHierarchyService(property, ont, inverse, comparator);
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getTree();

        String entityName = kit.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit)
                .withActiveObject(property)
                .withURLScheme(new RelationsURLScheme(kit, relationsHierarchyService).withQuery(request.getQueryString()));

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("hierarchy", propertyTree);
        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("type2", entityName);
        model.addAttribute("inverse", inverse);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/onannotationproperty/{propertyId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {

        OWLAnnotationProperty property = annotationPropertiesService.getOWLAnnotationPropertyFor(propertyId, kit);

        OWLOntology ont = kit.getActiveOntology();

        // property tree
        OWLAnnotationPropertyHierarchyService hierarchyService = new OWLAnnotationPropertyHierarchyService(
                ont,
                Comparator.comparing(o -> o.value.iterator().next()));
        Tree<OWLAnnotationProperty> propertyTree = hierarchyService.getPrunedTree(property);

        Comparator<Tree<OWLNamedIndividual>> comparator =// (orderBy == null) ?
                Comparator.comparing(o -> o.value.iterator().next());// :
//                new PropComparator(annotationPropertiesService.getOWLAnnotationPropertyFor(orderBy, kit), ont);

        // relations tree
        AnnotationsHierarchyService relationsHierarchyService = new AnnotationsHierarchyService(property, ont, inverse, comparator);
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getTree();

        String entityName = kit.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit)
                .withActiveObject(property)
                .withURLScheme(new AnnotationRelationsURLScheme(kit, relationsHierarchyService).withQuery(request.getQueryString()));

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("hierarchy", propertyTree);
        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("type2", entityName);
        model.addAttribute("inverse", inverse);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @GetMapping(value = "/onannotationproperty/{propertyId}/withindividual/{individualId}")
    public String getRelationsForAnnotationProperty(@PathVariable final String propertyId,
                                                    @PathVariable final String individualId,
                                                    @RequestParam(defaultValue = "false") final boolean inverse,
                                                    @RequestParam final @Nullable String orderBy,
                                                    final Model model,
                                                    HttpServletRequest request) throws NotFoundException {

        OWLAnnotationProperty property = annotationPropertiesService.getOWLAnnotationPropertyFor(propertyId, kit);
        Set<OWLOntology> ontologies = kit.getOntologies();
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ontologies);

        OWLOntology ont = kit.getActiveOntology();

        // property tree
        OWLAnnotationPropertyHierarchyService hierarchyService = new OWLAnnotationPropertyHierarchyService(
                ont,
                Comparator.comparing(o -> o.value.iterator().next()));
        Tree<OWLAnnotationProperty> propertyTree = hierarchyService.getPrunedTree(property);

        Comparator<Tree<OWLNamedIndividual>> comparator =// (orderBy == null) ?
                Comparator.comparing(o -> o.value.iterator().next());// :
//                new PropComparator(annotationPropertiesService.getOWLAnnotationPropertyFor(orderBy, kit), ont);

        // relations tree
        AnnotationsHierarchyService relationsHierarchyService = new AnnotationsHierarchyService(property, ont, inverse, comparator);
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getPrunedTree(individual);

        ShortFormProvider sfp = kit.getShortFormProvider();

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit)
                .withActiveObjects(ImmutableSet.of(property, individual))
                .withURLScheme(new AnnotationRelationsURLScheme(kit, relationsHierarchyService).withQuery(request.getQueryString()));

        List<Characteristic> characteristics = individualsService.getCharacteristics(individual, ontologies, kit.getComparator());

        model.addAttribute("title", sfp.getShortForm(individual) + " (Individual)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("iri", individual.getIRI());
        model.addAttribute("hierarchy", propertyTree);

        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("type2", sfp.getShortForm(property));
        model.addAttribute("inverse", inverse);

        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @GetMapping(value = "/onproperty/{propertyId}/withindividual/{individualId}")
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @PathVariable final String individualId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model,
                                          HttpServletRequest request) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);
        Set<OWLOntology> ontologies = kit.getOntologies();
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ontologies);

        OWLOntology ont = kit.getActiveOntology();

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        // property tree
        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(r,
                Comparator.comparing(o -> o.value.iterator().next()));
        Tree<OWLObjectPropertyExpression> propertyTree = hierarchyService.getPrunedTree(property);

        Comparator<Tree<OWLNamedIndividual>> comparator = (orderBy == null) ?
                Comparator.comparing(o -> o.value.iterator().next()) :
                new PropComparator(propertiesService.getOWLObjectPropertyFor(orderBy, kit), ont);

        // relations tree
        RelationsHierarchyService relationsHierarchyService = new RelationsHierarchyService(property, ont, inverse, comparator);
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getPrunedTree(individual);

        ShortFormProvider sfp = kit.getShortFormProvider();

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit)
                .withActiveObjects(ImmutableSet.of(property, individual))
                .withURLScheme(new RelationsURLScheme(kit, relationsHierarchyService).withQuery(request.getQueryString()));

        List<Characteristic> characteristics = individualsService.getCharacteristics(individual, ontologies, kit.getComparator());

        model.addAttribute("title", sfp.getShortForm(individual) + " (Individual)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("iri", individual.getIRI());
        model.addAttribute("hierarchy", propertyTree);

        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("type2", sfp.getShortForm(property));
        model.addAttribute("inverse", inverse);

        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/onproperty/{propertyId}/children")
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);

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

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, kit.getOntologies());

        OWLOntology ont = kit.getActiveOntology();

        Comparator<Tree<OWLNamedIndividual>> comparator = (orderBy == null) ?
                Comparator.comparing(o -> o.value.iterator().next()) :
                new PropComparator(propertiesService.getOWLObjectPropertyFor(orderBy, kit), ont);

        RelationsHierarchyService relHierarchy = new RelationsHierarchyService(property, ont, inverse, comparator);

        URLScheme urlScheme = new RelationsURLScheme(kit, relHierarchy).withQuery(request.getQueryString());

        model.addAttribute("t", relHierarchy.getChildren(individual));
        model.addAttribute("mos", new OWLHTMLRenderer(kit).withURLScheme(urlScheme));

        return "base :: tree";
    }
}
