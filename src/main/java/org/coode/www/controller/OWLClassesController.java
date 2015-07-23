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
import org.coode.www.service.hierarchy.OWLClassHierarchyService;
import org.coode.www.service.OWLClassesService;
import org.coode.www.service.OWLIndividualsService;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.net.URL;
import java.util.Comparator;

@Controller
@RequestMapping(value="/classes")
@SessionAttributes("kit")
public class OWLClassesController extends ApplicationController {

    @Autowired
    private OWLClassesService service;

    @Autowired
    private OWLIndividualsService individualsService;

    @RequestMapping(value="/", method=RequestMethod.GET)
    public String getOWLClasses(@ModelAttribute("kit") final OWLHTMLKit kit) throws OntServerException {

        OWLClass owlThing = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        String id = service.getIdFor(owlThing);

        return "redirect:/classes/" + id;
    }

    @RequestMapping(value="/{classId}", method=RequestMethod.GET)
    public String getOWLClass(@PathVariable final String classId,
                              @ModelAttribute("kit") final OWLHTMLKit kit,
                              final Model model) throws OntServerException, NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLClass>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLClass> prunedTree = hierarchyService.getPrunedTree(owlClass);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.of(owlClass));

        String entityName = owlServer.getShortFormProvider().getShortForm(owlClass);

        model.addAttribute("title", entityName + " (Class)");
        model.addAttribute("type", "Classes");
        model.addAttribute("iri", owlClass.getIRI().toString());
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", owlServer.getActiveOntology());
        model.addAttribute("hierarchy", prunedTree);
        model.addAttribute("ontologies", owlServer.getOntologies());
        model.addAttribute("characteristics", service.getCharacteristics(owlClass, kit));
        model.addAttribute("mos", owlRenderer);

        return "owlentity";
    }

    @RequestMapping(value="/{classId}/children", method=RequestMethod.GET)
    public String getChildren(@PathVariable final String classId,
                              @ModelAttribute("kit") final OWLHTMLKit kit,
                              final Model model) throws OntServerException, NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLClass>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLClass> prunedTree = hierarchyService.getChildren(owlClass);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.absent());

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }

    @RequestMapping(value="/{classId}/instances", method=RequestMethod.GET)
    public String getInstances(@PathVariable final String classId,
                               @ModelAttribute("kit") final OWLHTMLKit kit,
                               final Model model) throws OntServerException, NotFoundException {

        OWLEntity owlClass = service.getOWLClassFor(classId, kit);

        OWLServer owlServer = kit.getOWLServer();

        Comparator<Tree<OWLEntity>> comparator = (o1, o2) ->
                o1.value.iterator().next().compareTo(o2.value.iterator().next());

        OWLIndividualsByTypeHierarchyService hierarchyService =
                new OWLIndividualsByTypeHierarchyService(owlServer.getOWLReasoner(), comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getChildren(owlClass);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.absent());

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
