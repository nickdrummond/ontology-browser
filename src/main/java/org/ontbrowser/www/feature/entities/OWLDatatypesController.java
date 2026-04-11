package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.hierarchy.OWLDatatypeHierarchyService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.List;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE_STR;
import static org.ontbrowser.www.model.Tree.treeComparator;

@RestController
@RequestMapping(value = "/datatypes")
public class OWLDatatypesController {

    private final OWLHTMLKit kit;
    private final OWLDatatypesService service;
    private final CommonContent commonContent;
    private final CommonFragments commonFragments;

    public OWLDatatypesController(
            OWLHTMLKit kit,
            OWLDatatypesService service,
            CommonContent commonContent,
            CommonFragments commonFragments
    ) {
        this.kit = kit;
        this.service = service;
        this.commonContent = commonContent;
        this.commonFragments = commonFragments;
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

    @GetMapping(value = "/{datatypeId}")
    public ModelAndView getOWLDatatype(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request
    ) {
        var owlDatatype = kit.lookup().entityFor(datatypeId, ont, OWLDatatype.class);

        var hierarchyService = new OWLDatatypeHierarchyService(ont, treeComparator());

        var prunedTree = hierarchyService.getPrunedTree(owlDatatype);

        commonContent.addCommonContent(request.getQueryString(), model, ont);
        model.addAttribute("hierarchy", prunedTree);

        getOWLDatatypeFragment(datatypeId, ont, pageSize, with, model, request);

        return new ModelAndView("owlentity");
    }


    @GetMapping(value = "/{datatypeId}/fragment")
    public ModelAndView getOWLDatatypeFragment(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            final Model model,
            final HttpServletRequest request) {

        var entity = kit.lookup().entityFor(datatypeId, ont, OWLDatatype.class);

        String entityName = kit.getShortFormProvider().getShortForm(entity);
        String title = entityName + " (Datatype)";
        boolean inferred = false;

        String queryString = request.getQueryString();
        return commonFragments.buildCommonEntityFragment(service, entity, inferred, ont, with, model, queryString, title);
    }

    @GetMapping(value = "/{datatypeId}/children")
    public ModelAndView getChildren(
            @PathVariable final String datatypeId,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        var owlDatatype = kit.lookup().entityFor(datatypeId, ont, OWLDatatype.class);
        var hierarchyService = new OWLDatatypeHierarchyService(ont, treeComparator());
        var prunedTree = hierarchyService.getChildren(owlDatatype);

        model.addAttribute("t", prunedTree);

        return new ModelAndView("tree::tree");
    }
}
