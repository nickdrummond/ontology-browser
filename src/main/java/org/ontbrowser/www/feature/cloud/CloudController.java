package org.ontbrowser.www.feature.cloud;

import org.ontbrowser.www.feature.cloud.model.*;
import org.ontbrowser.www.controller.ApplicationController;
import org.ontbrowser.www.renderer.OWLHTMLRenderer;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@RestController
@RequestMapping(value = "/clouds")
@SessionAttributes("kit")
public class CloudController extends ApplicationController {

    @GetMapping(value = "/classes")
    public ModelAndView getClassesCloud(
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(ont.getImportsClosure());

        return cloud(ont, model, "Classes Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/individuals")
    public ModelAndView getIndividualsCloud(
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(ont.getImportsClosure());

        return cloud(ont, model, "Individuals Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/objectproperties")
    public ModelAndView getObjectPropertiesCloud(
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(ont.getImportsClosure());

        return cloud(ont, model, "Object Properties Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/dataproperties")
    public ModelAndView getDataPropertiesCloud(
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(ont.getImportsClosure());

        return cloud(ont, model, "Data Properties Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/annotationproperties")
    public ModelAndView getAnnotationPropertiesCloud(
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(ont.getImportsClosure());

        return cloud(ont, model, "Annotation Properties Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @GetMapping(value = "/datatypes")
    public ModelAndView getDatatypesCloud(
            @RequestParam(defaultValue = "${cloud.zoom.default}") int zoom,
            @RequestParam(defaultValue = "${cloud.threshold.default}") int threshold,
            @RequestParam(defaultValue="false") boolean normalise,
            @ModelAttribute final OWLOntology ont,
            final Model model) {

        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(ont.getImportsClosure());

        return cloud(ont, model, "Datatypes Usage Cloud", cloudModel, zoom, threshold, normalise);
    }

    @SuppressWarnings("SameReturnValue")
    public <T extends OWLEntity> ModelAndView cloud(final OWLOntology ont,
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

        OWLHTMLRenderer owlRenderer = rendererFactory.getHTMLRenderer(ont);

        model.addAttribute("title", title);
        model.addAttribute("ontologies", ont.getImportsClosure());
        model.addAttribute("ontologiesSfp", kit.getOntologySFP());
        model.addAttribute("helper", helper);
        model.addAttribute("mos", owlRenderer);

        return new ModelAndView("cloud");
    }
}
