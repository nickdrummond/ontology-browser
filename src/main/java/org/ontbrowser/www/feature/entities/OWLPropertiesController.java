package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import java.io.IOException;
import java.util.List;

import static org.ontbrowser.www.controller.Constants.DEFAULT_PAGE_SIZE_STR;

public class OWLPropertiesController<P extends OWLProperty> {

    private final OWLHTMLKit kit;
    private final PropertiesService<P> service;
    private final CommonContent commonContent;
    private final Class<P> clz;
    private final CommonFragments commonFragments;

    public OWLPropertiesController(
            OWLHTMLKit kit,
            PropertiesService<P> service,
            CommonContent commonContent,
            Class<P> clz,
            CommonFragments commonFragments
    ) {
        this.kit = kit;
        this.service = service;
        this.commonContent = commonContent;
        this.clz = clz;
        this.commonFragments = commonFragments;
    }

    @GetMapping(value = "/")
    public void getIndexOld(
            @RequestParam(required = false) final String ontId,
            final HttpServletResponse response
    ) throws IOException {
        String path = ServletUriComponentsBuilder.fromCurrentRequestUri().toUriString();
        response.sendRedirect(path + ".." + (ontId != null ? "?ontId=" + ontId : ""));
    }

    @GetMapping()
    public ModelAndView getIndex(
            @ModelAttribute OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        commonContent.addCommonContent(request.getQueryString(), model, ont);
        var hierarchyService = service.getHierarchyService(ont);
        model.addAttribute("hierarchy", hierarchyService.getTree());
        var pluralType = service.getEntityType().getPluralPrintName();
        model.addAttribute("title", pluralType);
        model.addAttribute("type", pluralType);
        return new ModelAndView("owlentity");
    }

    @GetMapping(value = "/{propertyId}")
    public ModelAndView getPage(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var prop = kit.lookup().entityFor(propertyId, ont, clz);

        commonContent.addCommonContent(request.getQueryString(), model, ont);
        model.addAttribute("hierarchy", service.getHierarchyService(ont).getPrunedTree(prop));
        model.addAttribute("type", service.getEntityType().getPluralPrintName());

        getFragment(propertyId, pageSize, with, ont, model, request);

        return new ModelAndView("owlentity");
    }

    @GetMapping(value = "/{propertyId}/fragment")
    public ModelAndView getFragment(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var entity = kit.lookup().entityFor(propertyId, ont, clz);

        String entityName = kit.getShortFormProvider().getShortForm(entity);
        String typeName = service.getEntityType().getPrintName();
        String title = entityName + " ( " + typeName + " )";

        boolean inferred = false;

        String queryString = request.getQueryString();

        return commonFragments.buildCommonEntityFragment(service, entity, inferred, ont, with, model, queryString, title);
    }

    @GetMapping(value = "/{propertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            final Model model
    ) {
        var prop = kit.lookup().entityFor(propertyId, ont, clz);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));

        return new ModelAndView("tree::children");
    }
}
