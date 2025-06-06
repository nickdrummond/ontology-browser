package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.hierarchy.OWLDatatypeHierarchyService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/datatypes")
public class OWLDatatypesController extends ApplicationController {

    private final OWLDatatypesService service;

    public OWLDatatypesController(
            OWLDatatypesService service) {
        this.service = service;
    }

    @GetMapping(value = "/")
    public void getOWLDatatypesOld(
            final HttpServletResponse response
    ) throws IOException {
        getOWLDatatypes(response);
    }

    @GetMapping()
    public void getOWLDatatypes(
            final HttpServletResponse response
    ) throws IOException {

        var df = kit.getOWLOntologyManager().getOWLDataFactory();

        var owlTopDatatype = df.getTopDatatype();

        String id = kit.lookup().getId(owlTopDatatype);

        response.sendRedirect("/datatypes/" + id);
    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{datatypeId}")
    public ModelAndView getOWLDatatype(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var owlDatatype = kit.lookup().entityFor(datatypeId, ont, OWLDatatype.class);

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(ont, treeComparator());

        Tree<OWLDatatype> prunedTree = hierarchyService.getPrunedTree(owlDatatype);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        model.addAttribute("hierarchy", prunedTree);

        getOWLDatatypeFragment(datatypeId, ont, pageSize, withOrEmpty, model, request, response);

        return new ModelAndView("owlentity");
    }


    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{datatypeId}/fragment")
    public ModelAndView getOWLDatatypeFragment(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false) List<With> with,
            final Model model,
            final HttpServletRequest request,
            final HttpServletResponse response) {

        var owlDatatype = kit.lookup().entityFor(datatypeId, ont, OWLDatatype.class);

        String entityName = kit.getShortFormProvider().getShortForm(owlDatatype);

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(owlDatatype);

        List<With> withOrEmpty = with != null ? with : Collections.emptyList();

        List<Characteristic> characteristics = service.getCharacteristics(owlDatatype, ont, kit.getComparator(), withOrEmpty, pageSize);

        model.addAttribute("title", entityName + " (Datatype)");
        model.addAttribute("type", "Datatypes");
        model.addAttribute("iri", owlDatatype.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request, withOrEmpty));

        response.addHeader("title", projectInfo.name() + ": " + entityName);

        return new ModelAndView("owlentityfragment");

    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{datatypeId}/children")
    public ModelAndView getChildren(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        var owlDatatype = kit.lookup().entityFor(datatypeId, ont, OWLDatatype.class);

        OWLDatatypeHierarchyService hierarchyService = new OWLDatatypeHierarchyService(ont, treeComparator());

        Tree<OWLDatatype> prunedTree = hierarchyService.getChildren(owlDatatype);

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont);

        model.addAttribute("t", prunedTree);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("base :: tree");
    }
}
