package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDatatypesService;
import org.coode.www.service.hierarchy.OWLDatatypeHierarchyService;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.Comparator;
import java.util.Set;

@Controller
@RequestMapping(value="/datatypes")
public class OWLDatatypesController extends ApplicationController {

    @Autowired
    private OWLDatatypesService service;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLDatatypes() {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();

        String id = service.getIdFor(owlTopDatatype);

        return "redirect:/datatypes/" + id;
    }


    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{propertyId}", method=RequestMethod.GET)
    public String getOWLDatatype(@PathVariable final String propertyId,
                                 final Model model) throws NotFoundException {

        OWLDatatype owlDatatype = service.getOWLDatatypeFor(propertyId, kit);

        Set<OWLOntology> ontologies = kit.getOntologies();

        Comparator<Tree<OWLDatatype>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(
                kit.getOWLOntologyManager().getOWLDataFactory(),
                ontologies,
                comparator);

        Tree<OWLDatatype> prunedTree = hierarchyService.getPrunedTree(owlDatatype);

        String entityName = kit.getShortFormProvider().getShortForm(owlDatatype);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit).withActiveObject(owlDatatype);

        model.addAttribute("title", entityName + " (Datatype)");
        model.addAttribute("type", "Datatypes");
        model.addAttribute("iri", owlDatatype.getIRI());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("characteristics", service.getCharacteristics(owlDatatype, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @SuppressWarnings("SameReturnValue")
    @RequestMapping(value="/{propertyId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String propertyId,
                              final Model model) throws NotFoundException {

        OWLDatatype property = service.getOWLDatatypeFor(propertyId, kit);

        Comparator<Tree<OWLDatatype>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(
                kit.getOWLOntologyManager().getOWLDataFactory(),
                kit.getActiveOntologies(),
                comparator);

        Tree<OWLDatatype> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
