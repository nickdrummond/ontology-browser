package org.ontbrowser.www.feature.entities;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.graph.GraphURLScheme;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.url.ComponentPagingURIScheme;
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

public class OWLPropertiesController<P extends OWLProperty> extends ApplicationController {

    private final PropertiesService<P> service;
    private final CommonContent commonContent;
    private final Class<P> clz;

    public OWLPropertiesController(
            OWLHTMLKit kit,
            PropertiesService<P> service,
            CommonContent commonContent,
            Class<P> clz
    ) {
        super(kit);
        this.service = service;
        this.commonContent = commonContent;
        this.clz = clz;
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

        var owlRenderer = rendererFactory.getHTMLRenderer(ont);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("owlentity");
    }

    @SuppressWarnings("SameReturnValue")
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

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/fragment")
    public ModelAndView getFragment(
            @PathVariable final String propertyId,
            @RequestParam(required = false, defaultValue = DEFAULT_PAGE_SIZE_STR) int pageSize,
            @RequestParam(required = false, defaultValue = "") List<With> with,
            @ModelAttribute OWLOntology ont,
            final Model model,
            final HttpServletRequest request
    ) {
        var prop = kit.lookup().entityFor(propertyId, ont, clz);

        String entityName = kit.getShortFormProvider().getShortForm(prop);

        var owlRenderer = rendererFactory.getHTMLRenderer(ont).withActiveObject(prop);
        
        var characteristics = service.getCharacteristics(prop, ont, kit.getComparator(), with, pageSize);

        if (projectInfo.activeProfiles().contains("graph")) {
            var mos = new MOSStringRenderer(kit.getFinder(), ont);
            model.addAttribute("graphLink", new GraphURLScheme(mos).getURLForOWLObject(prop, ont));
        }

        String typeName = service.getEntityType().getPrintName();
        model.addAttribute("title", entityName + " ( " + typeName + " )");
        model.addAttribute("iri", prop.getIRI());
        model.addAttribute("characteristics", characteristics);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("mos", owlRenderer);
        model.addAttribute("pageURIScheme", new ComponentPagingURIScheme(request.getQueryString(), with));

        return new ModelAndView("owlentityfragment");

    }

    @SuppressWarnings("SameReturnValue")
    @GetMapping(value = "/{propertyId}/children")
    public ModelAndView getChildren(
            @PathVariable final String propertyId,
            @ModelAttribute final OWLOntology ont,
            final Model model
    ) {
        var prop = kit.lookup().entityFor(propertyId, ont, clz);

        model.addAttribute("t", service.getHierarchyService(ont).getChildren(prop));
        model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));

        return new ModelAndView("tree::children");
    }
}
