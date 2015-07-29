package org.coode.www.controller;

import com.google.common.base.Optional;
import org.coode.owl.mngr.OWLServer;
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
@SessionAttributes("kit")
public class OWLObjectPropertiesController extends ApplicationController {

    @Autowired
    private OWLObjectPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLObjectProperties(@ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();

        String id = service.getIdFor(owlTopObjectProperty);

        return "redirect:/objectproperties/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLObjectProperty(@PathVariable final String propertyId,
                                       @ModelAttribute("kit") final OWLHTMLKit kit,
                                       final Model model) throws OntServerException, NotFoundException {

        OWLObjectProperty property = service.getOWLObjectPropertyFor(propertyId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLObjectPropertyHierarchyService hierarchyService =
                new OWLObjectPropertyHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getPrunedTree(property);

        String entityName = owlServer.getShortFormProvider().getShortForm(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.of(property));

        model.addAttribute("title", entityName + " (Object Property)");
        model.addAttribute("type", "Object Properties");
        model.addAttribute("iri", property.getIRI());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", owlServer.getActiveOntology());
        model.addAttribute("ontologies", owlServer.getOntologies());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", service.getCharacteristics(property, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              @ModelAttribute("kit") final OWLHTMLKit kit,
                              final Model model) throws OntServerException, NotFoundException {

        OWLObjectProperty property = service.getOWLObjectPropertyFor(propertyId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLObjectPropertyExpression>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLObjectPropertyHierarchyService hierarchyService =
                new OWLObjectPropertyHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLObjectPropertyExpression> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.absent());

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
