package org.coode.www.controller;

import org.coode.www.cloud.*;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;

import java.util.Set;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@Controller
@RequestMapping(value = "/clouds")
@SessionAttributes("kit")
public class CloudController extends ApplicationController {

    @GetMapping(value = "/classes")
    public String getClassesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                  @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
                                  @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
                                  @RequestParam(defaultValue="false") boolean normalise,
                                  final Model model) {

        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(kit.getOntologies());

        return cloud(kit, model, "Classes Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/individuals")
    public String getIndividualsCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                      @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
                                      @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
                                      @RequestParam(defaultValue="false") boolean normalise,
                                      final Model model) {

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(kit.getOntologies());

        return cloud(kit, model, "Individuals Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/objectproperties")
    public String getObjectPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                           @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
                                           @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
                                           @RequestParam(defaultValue="false") boolean normalise,
                                           final Model model) {

        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(kit.getOntologies());

        return cloud(kit, model, "Object Properties Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/dataproperties")
    public String getDataPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                         @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
                                         @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
                                         @RequestParam(defaultValue="false") boolean normalise,
                                         final Model model) {

        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(kit.getOntologies());

        return cloud(kit, model, "Data Properties Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/annotationproperties")
    public String getAnnotationPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                               @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
                                               @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
                                               @RequestParam(defaultValue="false") boolean normalise,
                                               final Model model) {

        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(kit.getOntologies());

        return cloud(kit, model, "Annotation Properties Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/datatypes")
    public String getDatatypesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                    @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
                                    @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
                                    @RequestParam(defaultValue="false") boolean normalise,
                                    final Model model) {

        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(kit.getOntologies());

        return cloud(kit, model, "Datatypes Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @SuppressWarnings("SameReturnValue")
    public <T extends OWLEntity>String cloud(final OWLHTMLKit kit,
                                             final Model model,
                                             final String title,
                                             final CloudModel<T> cloudModel,
                                             final int zoom,
                                             final int threshold,
                                             boolean normalise) {

        CloudHelper<T> helper = new CloudHelper<>(cloudModel);
        helper.setZoom(zoom);
        helper.setThreshold(threshold);
        helper.setNormalise(normalise);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit);

        Set<OWLOntology> ontologies = kit.getOntologies();

        model.addAttribute("title", title);
        model.addAttribute("activeOntology", kit.getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("helper", helper);
        model.addAttribute("mos", owlRenderer);

        return "cloud";
    }
}
