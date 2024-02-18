package org.coode.www.controller;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.OWLDatatypesService;
import org.coode.www.service.hierarchy.OWLDatatypeHierarchyService;
import org.coode.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@Controller
@RequestMapping(value="/datatypes")
public class OWLDatatypesController extends ApplicationController {

    @Autowired
    private OWLDatatypesService service;

    @GetMapping(value="/")
    public String getOWLDatatypes() {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();

        String id = service.getIdFor(owlTopDatatype);

        return "redirect:/datatypes/" + id;
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/{datatypeId}")
    public String getOWLDatatype(
        @PathVariable final String datatypeId,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false) List<With> with,
        final HttpServletRequest request,
        final Model model
    ) throws NotFoundException {

        OWLDatatype owlDatatype = service.getOWLDatatypeFor(datatypeId, kit);

        OWLOntology ont = kit.getActiveOntology();

        Comparator<Tree<OWLDatatype>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(ont, comparator);

        Tree<OWLDatatype> prunedTree = hierarchyService.getPrunedTree(owlDatatype);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        model.addAttribute("hierarchy", prunedTree);

        getOWLDatatypeFragment(datatypeId, pageSize, withOrEmpty, request, model);

        return "owlentity";
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/fragment/{datatypeId}")
    public String getOWLDatatypeFragment(
            @PathVariable final String datatypeId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final HttpServletRequest request,
            final Model model
    ) throws NotFoundException {

        OWLDatatype owlDatatype = service.getOWLDatatypeFor(datatypeId, kit);

        OWLOntology ont = kit.getActiveOntology();

        String entityName = kit.getShortFormProvider().getShortForm(owlDatatype);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(kit.getActiveOntology()).withActiveObject(owlDatatype);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = service.getCharacteristics(owlDatatype, ont, kit.getComparator(), withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Datatype)");
        model.addAttribute("type", "Datatypes");
        model.addAttribute("iri", owlDatatype.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        return "owlentityfragment";
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/children")
    public String getChildren(
            @PathVariable final String propertyId,
            final Model model) throws NotFoundException {

        OWLDatatype property = service.getOWLDatatypeFor(propertyId, kit);

        Comparator<Tree<OWLDatatype>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLOntology ont = kit.getActiveOntology();

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(ont, comparator);

        Tree<OWLDatatype> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return "base :: tree";
    }
}
