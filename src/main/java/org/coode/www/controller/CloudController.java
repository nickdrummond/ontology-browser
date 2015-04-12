package org.coode.www.controller;

import org.coode.html.doclet.CloudDoclet;
import org.coode.www.cloud.*;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;

import javax.servlet.http.HttpServletRequest;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@Controller
@RequestMapping(value = "/clouds")
public class CloudController extends ApplicationController {

    @Value("${cloud.threshold.default}")
    private int threshold;

    @Value("${cloud.zoom.default}")
    private int zoom;

    @RequestMapping(value = "/classes")
    public String getClassesCloud(
            @RequestParam(required = false) final String label,
            final Model model,
            final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/individuals")
    public String getIndividualsCloud(
            @RequestParam(required = false) final String label,
            final Model model,
            final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }


    @RequestMapping(value = "/objectproperties")
    public String getObjectPropertiesCloud(
            @RequestParam(required = false) final String label,
            final Model model,
            final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/dataproperties")
    public String getDataPropertiesCloud(
            @RequestParam(required = false) final String label,
            final Model model,
            final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/annotationproperties")
    public String getAnnotationPropertiesCloud(
            @RequestParam(required = false) final String label,
            final Model model,
            final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/datatypes")
    public String getDatatypesCloud(
            @RequestParam(required = false) final String label,
            final Model model,
            final HttpServletRequest request) throws OntServerException {

        final OWLHTMLKit kit = sessionManager.getHTMLKit(request, label);

        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }
}
