package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDataPropertiesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.OWLDataPropertyHierarchyService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.Comparator;

@Controller
@RequestMapping(value="/dataproperties")
public class OWLDataPropertiesController extends ApplicationController {

    @Autowired
    private OWLDataPropertiesService service;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLDataProperties() {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();

        String id = service.getIdFor(owlTopDataProperty);

        return "redirect:/dataproperties/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLDataProperty(@PathVariable final String propertyId,
                                     final Model model) throws NotFoundException {

        OWLDataProperty owlDataProperty = service.getOWLDataPropertyFor(propertyId, kit);

        Comparator<Tree<OWLDataProperty>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(kit.getActiveOntology());

        OWLDataPropertyHierarchyService hierarchyService = new OWLDataPropertyHierarchyService(r, comparator);

        Tree<OWLDataProperty> prunedTree = hierarchyService.getPrunedTree(owlDataProperty);

        String entityName = kit.getShortFormProvider().getShortForm(owlDataProperty);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit).withActiveObject(owlDataProperty);

        model.addAttribute("title", entityName + " (Data Property)");
        model.addAttribute("type", "Data Properties");
        model.addAttribute("iri", owlDataProperty.getIRI());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", service.getCharacteristics(owlDataProperty, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLDataProperty property = service.getOWLDataPropertyFor(propertyId, kit);

        Comparator<Tree<OWLDataProperty>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLReasoner r = reasonerFactoryService.getToldReasoner(kit.getActiveOntology());

        OWLDataPropertyHierarchyService hierarchyService = new OWLDataPropertyHierarchyService(r, comparator);

        Tree<OWLDataProperty> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
