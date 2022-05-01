package org.coode.www.controller;

import org.coode.www.cloud.*;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.SessionAttributes;

import java.util.Optional;
import java.util.Set;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@Controller
@RequestMapping(value = "/clouds")
@SessionAttributes("kit")
public class CloudController extends ApplicationController {

    @Value("${cloud.threshold.default}")
    private int thresholdDefault;

    @Value("${cloud.zoom.default}")
    private static int zoomDefault;
//
//    @RequestMapping(value = "/classes")
//    public String getClassesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
//                                  @RequestParam(required=false) Integer zoom,
//                                  @RequestParam(required=false) Integer threshold,
//                                  final Model model) throws OntServerException {
//        if (zoom == null) { zoom = zoomDefault; }
//        if (threshold == null) { threshold = thresholdDefault; }
//        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(kit.getOntologies());
//        return cloud(kit, model, "Classes Usage Cloud", cloudModel, zoom, threshold);
//    }

    @RequestMapping(value = "/individuals")
    public String getIndividualsCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                      @RequestParam(required=false) Integer zoom,
                                      @RequestParam(required=false) Integer threshold,
                                      @RequestParam(required=false, defaultValue="false") boolean normalise,
                                      final Model model) throws OntServerException {

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(kit.getOntologies());

        if (zoom == null) { zoom = zoomDefault; }
        if (threshold == null) { threshold = thresholdDefault; }
        CloudHelper<OWLNamedIndividual> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        if (normalise) {helper.setNormalise(normalise);}
        return cloud(kit, model, "Individuals Usage Cloud", helper);
    }
//
//    @RequestMapping(value = "/objectproperties")
//    public String getObjectPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
//                                           @RequestParam(required=false) Integer zoom,
//                                           @RequestParam(required=false) Integer threshold,
//                                           final Model model) throws OntServerException {
//        if (zoom == null) { zoom = zoomDefault; }
//        if (threshold == null) { threshold = thresholdDefault; }
//        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(kit.getOntologies());
//        return cloud(kit, model, "Object Properties Usage Cloud", cloudModel, zo);
//    }
//
//    @RequestMapping(value = "/dataproperties")
//    public String getDataPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
//                                         @RequestParam(required=false) Integer zoom,
//                                         @RequestParam(required=false) Integer threshold,
//                                         final Model model) throws OntServerException {
//        if (zoom == null) { zoom = zoomDefault; }
//        if (threshold == null) { threshold = thresholdDefault; }
//        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(kit.getOntologies());
//        return cloud(kit, model, "Data Properties Usage Cloud", cloudModel, zoom, threshold);
//    }
//
//    @RequestMapping(value = "/annotationproperties")
//    public String getAnnotationPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
//                                               @RequestParam(required=false) Integer zoom,
//                                               @RequestParam(required=false) Integer threshold,
//                                               final Model model) throws OntServerException {
//        if (zoom == null) { zoom = zoomDefault; }
//        if (threshold == null) { threshold = thresholdDefault; }
//        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(kit.getOntologies());
//        return cloud(kit, model, "Annotation Properties Usage Cloud", cloudModel, zoom, threshold);
//    }
//
//    @RequestMapping(value = "/datatypes")
//    public String getDatatypesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
//                                    @RequestParam(required=false) Integer zoom,
//                                    @RequestParam(required=false) Integer threshold,
//                                    final Model model) throws OntServerException {
//        if (zoom == null) { zoom = zoomDefault; }
//        if (threshold == null) { threshold = thresholdDefault; }
//        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(kit.getOntologies());
//        return cloud(kit, model, "Datatypes Usage Cloud", cloudModel, zoom, threshold);
//    }

    public <T extends OWLEntity>String cloud(final OWLHTMLKit kit,
                                                    final Model model,
                                                    final String title,
                                                    final CloudHelper helper) {

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.empty());

        Set<OWLOntology> ontologies = kit.getOntologies();

        model.addAttribute("title", title);
        model.addAttribute("activeOntology", kit.getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("helper", helper);
        model.addAttribute("mos", owlRenderer);

        return "cloud";
    }
}
