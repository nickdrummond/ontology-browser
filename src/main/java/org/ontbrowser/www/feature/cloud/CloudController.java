package org.ontbrowser.www.feature.cloud;

import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.feature.cloud.model.CloudModelFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import static org.ontbrowser.www.util.OWLUtils.getEntityTypeFromPath;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@RestController
@RequestMapping(value = "/clouds")
public class CloudController extends ApplicationController {

    @GetMapping(value = "/{entityType}")
    public ModelAndView getUsageCloud(
            @PathVariable String entityType,
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @RequestParam(required = false, defaultValue = "INCLUDED") final Imports imports,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        var et = getEntityTypeFromPath(entityType);

        var cloudModel = CloudModelFactory.getUsageCloud(et, ont, imports);

        var title = et.getPluralPrintName() + " Usage Cloud";

        var helper = new CloudHelper<>(cloudModel);
        helper.setZoom(zoom);
        helper.setThreshold(threshold);
        helper.setNormalise(normalise);

        var owlRenderer = rendererFactory.getHTMLRenderer(ont);

        model.addAttribute("title", title);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("helper", helper);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("cloud");
    }
}
