package org.coode.www.controller;

import com.google.common.collect.ImmutableSet;
import org.coode.html.url.RelationsURLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Characteristic;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.RelationsHierarchyService;
import org.coode.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import javax.annotation.Nullable;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

@Controller
@RequestMapping(value="/relations")
public class OWLRelationsController extends ApplicationController {

    @Autowired
    private OWLObjectPropertiesService propertiesService;

    @Autowired
    private OWLIndividualsService individualsService;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getRelations() {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = propertiesService.getIdFor(owlTopObjectProperty);

        return "redirect:/relations/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);
        OWLObjectProperty orderByProperty = (orderBy == null) ? null : propertiesService.getOWLObjectPropertyFor(orderBy, kit);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(kit.getActiveOntology());

        // property tree
        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(r,
                Comparator.comparing(o -> o.value.iterator().next()));
        Tree<OWLObjectPropertyExpression> propertyTree = hierarchyService.getPrunedTree(property);

        Set<OWLOntology> ontologies = kit.getOntologies();

        // relations tree
        RelationsHierarchyService relationsHierarchyService = new RelationsHierarchyService(
                property, ontologies, inverse, orderByProperty,
                Comparator.comparing(o -> o.value.iterator().next())); // TODO order by property
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getTree();

        String entityName = kit.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, property);
        owlRenderer.setURLScheme(new RelationsURLScheme(kit, propertyId, inverse));

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("type2", entityName);
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("hierarchy", propertyTree);
        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @RequestMapping(value="/{propertyId}/{individualId}", method=RequestMethod.GET)
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @PathVariable final String individualId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);
        OWLObjectProperty orderByProperty = (orderBy == null) ? null : propertiesService.getOWLObjectPropertyFor(orderBy, kit);
        Set<OWLOntology> ontologies = kit.getOntologies();
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ontologies);

        OWLReasoner r = reasonerFactoryService.getToldReasoner(kit.getActiveOntology());

        // property tree
        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(r,
                Comparator.comparing(o -> o.value.iterator().next()));
        Tree<OWLObjectPropertyExpression> propertyTree = hierarchyService.getPrunedTree(property);

        // relations tree
        RelationsHierarchyService relationsHierarchyService = new RelationsHierarchyService(
                property, ontologies, inverse, orderByProperty,
                Comparator.comparing(o -> o.value.iterator().next())); // TODO order by property
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getPrunedTree(individual);

        ShortFormProvider sfp = kit.getShortFormProvider();
        String entityName = sfp.getShortForm(individual);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, ImmutableSet.of(property, individual));
        owlRenderer.setURLScheme(new RelationsURLScheme(kit, propertyId, inverse));

        List<Characteristic> characteristics = individualsService.getCharacteristics(individual, ontologies, kit.getComparator(), sfp);

        model.addAttribute("title", entityName + " (Individual)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("type2", sfp.getShortForm(property));
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("hierarchy", propertyTree);
        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(kit.getActiveOntology());

        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(r, comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }

    @RequestMapping(value="/{propertyId}/{individualId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              @PathVariable final String individualId,
                              @RequestParam(defaultValue = "false") final boolean inverse,
                              @RequestParam final @Nullable String orderBy,
                              final Model model) throws NotFoundException {

        OWLObjectProperty property = propertiesService.getOWLObjectPropertyFor(propertyId, kit);
        OWLObjectProperty orderByProperty = (orderBy == null) ? null : propertiesService.getOWLObjectPropertyFor(orderBy, kit);
        Set<OWLOntology> ontologies = kit.getOntologies();
        OWLNamedIndividual individual = individualsService.getOWLIndividualFor(individualId, ontologies);

        // relations tree
        RelationsHierarchyService relationsHierarchyService = new RelationsHierarchyService(
                property, ontologies, inverse, orderByProperty,
                Comparator.comparing(o -> o.value.iterator().next())); // TODO order by property
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getChildren(individual);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit); // no active entity, otherwise it highlights the expanding node
        owlRenderer.setURLScheme(new RelationsURLScheme(kit, propertyId, inverse));

        model.addAttribute("t", relationsTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
