package org.coode.www.controller;

import com.google.common.base.Optional;
import org.coode.html.doclet.NodeDoclet;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.owl.mngr.OWLServer;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDataPropertiesService;
import org.coode.www.service.hierarchy.OWLDataPropertyHierarchyService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.net.URL;
import java.util.Comparator;

@Controller
@RequestMapping(value="/dataproperties")
@SessionAttributes("kit")
public class OWLDataPropertiesController extends ApplicationController {

    @Autowired
    private OWLDataPropertiesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLDataProperties(@ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        final OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();

        String id = service.getIdFor(owlTopDataProperty);

        return "redirect:/dataproperties/" + id;
    }


    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLDataProperty(@PathVariable final String propertyId,
                                     @ModelAttribute("kit") final OWLHTMLKit kit,
                                     final Model model) throws OntServerException, NotFoundException {

        OWLDataProperty owlDataProperty = service.getOWLDataPropertyFor(propertyId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLDataProperty>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLDataPropertyHierarchyService hierarchyService =
                new OWLDataPropertyHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLDataProperty> prunedTree = hierarchyService.getPrunedTree(owlDataProperty);

        String entityName = owlServer.getShortFormProvider().getShortForm(owlDataProperty);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.of(owlDataProperty));

        model.addAttribute("title", entityName + " (Data Property)");
        model.addAttribute("type", "Data Properties");
        model.addAttribute("iri", owlDataProperty.getIRI());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", owlServer.getActiveOntology());
        model.addAttribute("ontologies", owlServer.getOntologies());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", service.getCharacteristics(owlDataProperty, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @ResponseBody
    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              @ModelAttribute("kit") final OWLHTMLKit kit,
                              @RequestHeader final URL referer) throws OntServerException, NotFoundException {

        OWLDataProperty property = service.getOWLDataPropertyFor(propertyId, kit);
        HierarchyProvider<OWLDataProperty> hp = service.getHierarchyProvider(kit);
        NodeDoclet<OWLDataProperty> nodeDoclet = new NodeDoclet<OWLDataProperty>(kit, property, hp);
        nodeDoclet.setUserObject(null); // not sure why wee need this, but otherwise no children

        return renderDoclets(referer, nodeDoclet);
    }
}
