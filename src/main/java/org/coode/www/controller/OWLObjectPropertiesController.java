package org.coode.www.controller;

import java.util.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLObjectPropertiesService;
import org.coode.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.Comparator;

@Controller
@RequestMapping(value="/objectproperties")
public class OWLObjectPropertiesController extends ApplicationController {

    @Autowired
    private OWLObjectPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLObjectProperties() throws OntServerException {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = service.getIdFor(owlTopObjectProperty);

        return "redirect:/objectproperties/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLObjectProperty(@PathVariable final String propertyId,
                                       final Model model) throws OntServerException, NotFoundException {

        OWLObjectProperty property = service.getOWLObjectPropertyFor(propertyId, kit);

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLObjectPropertyHierarchyService hierarchyService =
                new OWLObjectPropertyHierarchyService(kit.getOWLReasoner(), comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getPrunedTree(property);

        String entityName = kit.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.of(property));

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Object Properties");
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", service.getCharacteristics(property, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws OntServerException, NotFoundException {

        OWLObjectProperty property = service.getOWLObjectPropertyFor(propertyId, kit);

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLObjectPropertyHierarchyService hierarchyService =
                new OWLObjectPropertyHierarchyService(kit.getOWLReasoner(), comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.empty());

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
