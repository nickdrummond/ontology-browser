package org.coode.www.controller;

import com.google.common.collect.ImmutableSet;
import org.coode.html.url.RelationPropertyURLScheme;
import org.coode.html.url.RelationsURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Characteristic;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.coode.www.service.hierarchy.PropComparator;
import org.coode.www.service.hierarchy.RelationsHierarchyService;
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
import javax.servlet.http.HttpServletRequest;
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

        return "redirect:/relations/onproperty/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/onproperty/{propertyId}", method=RequestMethod.GET)
    public String getRelationsForProperty(@PathVariable final String propertyId,
                                          @RequestParam(defaultValue = "false") final boolean inverse,
                                          @RequestParam final @Nullable String orderBy,
                                          final Model model) throws NotFoundException {

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
        RelationsHierarchyService relationsHierarchyService = new RelationsHierarchyService(property, ont, inverse,comparator);
        Tree<OWLNamedIndividual> relationsTree = relationsHierarchyService.getTree();

        String entityName = kit.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit)
                .withActiveObject(property)
                .withURLScheme(new RelationsURLScheme(kit, relationsHierarchyService));

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

    @RequestMapping(value="/onproperty/{propertyId}/withindividual/{individualId}", method=RequestMethod.GET)
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

        List<Characteristic> characteristics = individualsService.getCharacteristics(individual, ontologies, kit.getComparator(), sfp);

        model.addAttribute("title", sfp.getShortForm(individual) + " (Individual)");
        model.addAttribute("type", "Relations on");
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("hierarchy", propertyTree);

        model.addAttribute("hierarchy2", relationsTree);
        model.addAttribute("type2", sfp.getShortForm(property));
        model.addAttribute("inverse", inverse);

        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/onproperty/{propertyId}/children", method=RequestMethod.GET)
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

    @RequestMapping(value="/onproperty/{propertyId}/withindividual/{individualId}/children", method=RequestMethod.GET)
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
