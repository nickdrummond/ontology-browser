package org.ontbrowser.www.feature.cloud;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.controller.CommonContent;
import org.ontbrowser.www.feature.cloud.model.CloudModelFactory;
import org.ontbrowser.www.feature.hierarchy.OWLOntologyHierarchyService;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import static org.ontbrowser.www.model.Tree.treeComparator;
import static org.ontbrowser.www.util.OWLUtils.getEntityTypeFromPath;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@RestController
@RequestMapping(value = "/clouds")
public class CloudController extends ApplicationController {

    private final CommonContent commonContent;

    public CloudController(CommonContent commonContent) {
        this.commonContent = commonContent;
    }

    @GetMapping(value = "/{entityType}")
    public ModelAndView getUsageCloud(
            @PathVariable String entityType,
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue = "true") boolean normalise,
            @RequestParam(required = false, defaultValue = "INCLUDED") final Imports imports,
            @ModelAttribute final OWLOntology ont,
            HttpServletRequest request,
            final Model model
    ) {
        var et = getEntityTypeFromPath(entityType);

        var cloudModel = CloudModelFactory.getUsageCloud(et, ont, imports);

        var title = et.getPluralPrintName() + " usage";

        commonContent.addCommonContent(request, model, ont);

        var helper = new CloudHelper<>(cloudModel);
        helper.setZoom(zoom);
        helper.setThreshold(threshold);
        helper.setNormalise(normalise);

        var hierarchyService = new OWLOntologyHierarchyService(kit.getRootOntology(), treeComparator());
        var ontologyTree = hierarchyService.getPrunedTree(ont);
        model.addAttribute("hierarchy", ontologyTree);

        // All ontologies link to the existing page with an ont filter
        var customRenderer = rendererFactory
                .getHTMLRenderer(ont)
                .withActiveObject(ont)
                .withURLScheme(owlObject -> {
                    if (owlObject instanceof OWLOntology ontLink) {
                        return request.getServletPath() + "?imports=EXCLUDED&ontId=" + ontLink.getOntologyID().hashCode();
                    }
                    return kit.getURLScheme().getURLForOWLObject(owlObject);
                });
        model.addAttribute("mos", customRenderer);

        model.addAttribute("title", title);
        model.addAttribute("helper", helper);

        return new ModelAndView("cloud");
    }
}
