package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.paging.With;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLClassesService;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.hierarchy.OWLClassHierarchyService;
import org.coode.www.service.hierarchy.OWLIndividualsByTypeHierarchyService;
import org.coode.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

@Controller
@RequestMapping(value="/classes")
public class OWLClassesController extends ApplicationController {

    @Autowired
    private OWLClassesService service;

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @GetMapping(value="/")
    public String getOWLClasses() {

        OWLClass owlThing = kit.getOWLOntologyManager().getOWLDataFactory().getOWLThing();

        String id = service.getIdFor(owlThing);

        return "redirect:/classes/" + id;
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}")
    public String getOWLClass(
            @PathVariable final String classId,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, kit);

        Comparator<Tree<OWLClass>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntology ont = kit.getActiveOntology();

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);
        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, comparator);

        Tree<OWLClass> prunedTree = hierarchyService.getPrunedTree(owlClass);

        model.addAttribute("hierarchy", prunedTree);

        getOWLClassFragment(classId, with, model, request);

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/fragment")
    public String getOWLClassFragment(
            @PathVariable final String classId,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, kit);

        OWLOntology ont = kit.getActiveOntology();

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlClass);

        ShortFormProvider sfp = kit.getShortFormProvider();

        String entityName = sfp.getShortForm(owlClass);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = service.getCharacteristics(
                owlClass, ont, kit.getComparator(),
                withOrEmpty,
                DEFAULT_PAGE_SIZE);

        Set<OWLClass> namedSuperclasses = service.getNamedTypes(owlClass, ont);

        String supers = String.join(", ", namedSuperclasses.stream().map(sfp::getShortForm).toList());

        String title = entityName + (supers.isEmpty() ? "" : " (" + supers + ")");

        model.addAttribute("title", title);
        model.addAttribute("type", "Classes");
        model.addAttribute("iri", owlClass.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        return "owlentityfragment";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/children")
    public String getChildren(@PathVariable final String classId,
                              final Model model) throws NotFoundException {

        OWLClass owlClass = service.getOWLClassFor(classId, kit);

        Comparator<Tree<OWLClass>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntology ont = kit.getActiveOntology();

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLClassHierarchyService hierarchyService = new OWLClassHierarchyService(r, comparator);

        Tree<OWLClass> prunedTree = hierarchyService.getChildren(owlClass);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{classId}/instances")
    public String getInstances(@PathVariable final String classId,
                               final Model model) throws NotFoundException {

        OWLEntity owlClass = service.getOWLClassFor(classId, kit);

        Comparator<Tree<OWLEntity>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntology ont = kit.getActiveOntology();

        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);

        OWLIndividualsByTypeHierarchyService hierarchyService = new OWLIndividualsByTypeHierarchyService(r, comparator);

        Tree<OWLEntity> prunedTree = hierarchyService.getChildren(owlClass);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
