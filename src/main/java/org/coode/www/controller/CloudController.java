package org.coode.www.controller;

import org.coode.html.doclet.CloudDoclet;
import org.coode.www.cloud.*;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.SessionAttributes;

import javax.servlet.http.HttpServletRequest;

/**
 * Could have gone in each OWLEntity type Controller, but this is a bit tangential.
 */
@Controller
@RequestMapping(value = "/clouds")
@SessionAttributes("kit")
public class CloudController extends ApplicationController {

    @Value("${cloud.threshold.default}")
    private int threshold;

    @Value("${cloud.zoom.default}")
    private int zoom;

    @RequestMapping(value = "/classes")
    public String getClassesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                  final Model model,
                                  final HttpServletRequest request) throws OntServerException {

        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("title", "Classes Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/individuals")
    public String getIndividualsCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                      final Model model,
                                      final HttpServletRequest request) throws OntServerException {

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("title", "Individuals Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }


    @RequestMapping(value = "/objectproperties")
    public String getObjectPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                           final Model model,
                                           final HttpServletRequest request) throws OntServerException {

        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("title", "Object Properties Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/dataproperties")
    public String getDataPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                         final Model model,
                                         final HttpServletRequest request) throws OntServerException {

        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("title", "Data Properties Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/annotationproperties")
    public String getAnnotationPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                               final Model model,
                                               final HttpServletRequest request) throws OntServerException {

        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("title", "Annotation Properties Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/datatypes")
    public String getDatatypesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                    final Model model,
                                    final HttpServletRequest request) throws OntServerException {

        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(kit);

        CloudDoclet cloudDoclet = new CloudDoclet(kit);
        cloudDoclet.setModel(cloudModel);
        cloudDoclet.setComparator(kit.getOWLServer().getComparator());
        cloudDoclet.setThreshold(threshold);
        cloudDoclet.setZoom(zoom);

        model.addAttribute("title", "Datatypes Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", kit.getOWLServer().getOntologies());
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }
}
