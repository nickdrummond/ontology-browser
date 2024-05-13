package org.ontbrowser.www.controller;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.service.OWLDatatypesService;
import org.ontbrowser.www.service.hierarchy.OWLDatatypeHierarchyService;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

@RestController
@RequestMapping(value="/datatypes")
public class OWLDatatypesController extends ApplicationController {

    private final OWLDatatypesService service;

    public OWLDatatypesController(
            @Autowired OWLDatatypesService service) {
        this.service = service;
    }

    @GetMapping(value="/")
    public void getOWLDatatypes(
            final HttpServletResponse response
    ) throws IOException {

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();

        String id = service.getIdFor(owlTopDatatype);

        response.sendRedirect("/datatypes/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/{datatypeId}")
    public ModelAndView getOWLDatatype(
        @PathVariable final String datatypeId,
        @ModelAttribute final OWLOntology ont,
        @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
        @RequestParam(required = false) List<With> with,
        final Model model,
        final HttpServletRequest request,
        final HttpServletResponse response) throws NotFoundException {

        OWLDatatype owlDatatype = service.getOWLDatatypeFor(datatypeId, ont);

        Comparator<Tree<OWLDatatype>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(ont, comparator);

        Tree<OWLDatatype> prunedTree = hierarchyService.getPrunedTree(owlDatatype);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        model.addAttribute("hierarchy", prunedTree);

        getOWLDatatypeFragment(datatypeId, ont, pageSize, withOrEmpty, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value= "/{datatypeId}/fragment")
    public ModelAndView getOWLDatatypeFragment(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) throws NotFoundException {

        OWLDatatype owlDatatype = service.getOWLDatatypeFor(datatypeId, ont);

        String entityName = kit.getShortFormProvider().getShortForm(owlDatatype);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont).withActiveObject(owlDatatype);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = service.getCharacteristics(owlDatatype, ont, kit.getComparator(), withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Datatype)");
        model.addAttribute("type", "Datatypes");
        model.addAttribute("iri", owlDatatype.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.getName() + ": " + entityName);

        return new ModelAndView("owlentityfragment");

    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value="/{propertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            final Model model) throws NotFoundException {

        OWLDatatype property = service.getOWLDatatypeFor(propertyId, ont);

        Comparator<Tree<OWLDatatype>> comparator = Comparator.comparing(o -> o.value.iterator().next());

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(ont, comparator);

        Tree<OWLDatatype> prunedTree = hierarchyService.getChildren(property);

        OWLHTMLRenderer owlRenderer = rendererFactory.getRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base :: tree");
    }
}
