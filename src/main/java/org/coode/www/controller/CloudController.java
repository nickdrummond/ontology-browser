package org.coode.www.controller;

import org.coode.html.doclet.CloudDoclet;
import org.coode.html.url.URLScheme;
import org.coode.www.cloud.*;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.SessionAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Set;

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

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();
        ShortFormProvider sfp = kit.getOWLServer().getShortFormProvider();
        URLScheme urlScheme = kit.getURLScheme();

        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(ontologies, sfp, urlScheme);
        CloudHelper<OWLClass> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        CloudDoclet<OWLClass> cloudDoclet = new CloudDoclet<>(kit, helper);

        model.addAttribute("title", "Classes Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/individuals")
    public String getIndividualsCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                      final Model model,
                                      final HttpServletRequest request) throws OntServerException {

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();
        ShortFormProvider sfp = kit.getOWLServer().getShortFormProvider();
        URLScheme urlScheme = kit.getURLScheme();

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(ontologies, sfp, urlScheme);

        CloudHelper<OWLNamedIndividual> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        CloudDoclet<OWLNamedIndividual> cloudDoclet = new CloudDoclet<>(kit, helper);

        model.addAttribute("title", "Individuals Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }


    @RequestMapping(value = "/objectproperties")
    public String getObjectPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                           final Model model,
                                           final HttpServletRequest request) throws OntServerException {

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();
        ShortFormProvider sfp = kit.getOWLServer().getShortFormProvider();
        URLScheme urlScheme = kit.getURLScheme();

        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(ontologies, sfp, urlScheme);

        CloudHelper<OWLObjectProperty> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        CloudDoclet<OWLObjectProperty> cloudDoclet = new CloudDoclet<>(kit, helper);

        model.addAttribute("title", "Object Properties Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/dataproperties")
    public String getDataPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                         final Model model,
                                         final HttpServletRequest request) throws OntServerException {

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();
        ShortFormProvider sfp = kit.getOWLServer().getShortFormProvider();
        URLScheme urlScheme = kit.getURLScheme();

        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(ontologies, sfp, urlScheme);

        CloudHelper<OWLDataProperty> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        CloudDoclet<OWLDataProperty> cloudDoclet = new CloudDoclet<>(kit, helper);

        model.addAttribute("title", "Data Properties Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/annotationproperties")
    public String getAnnotationPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                               final Model model,
                                               final HttpServletRequest request) throws OntServerException {

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();
        ShortFormProvider sfp = kit.getOWLServer().getShortFormProvider();
        URLScheme urlScheme = kit.getURLScheme();

        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(ontologies, sfp, urlScheme);

        CloudHelper<OWLAnnotationProperty> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        CloudDoclet<OWLAnnotationProperty> cloudDoclet = new CloudDoclet<>(kit, helper);

        model.addAttribute("title", "Annotation Properties Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }

    @RequestMapping(value = "/datatypes")
    public String getDatatypesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                    final Model model,
                                    final HttpServletRequest request) throws OntServerException {

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();
        ShortFormProvider sfp = kit.getOWLServer().getShortFormProvider();
        URLScheme urlScheme = kit.getURLScheme();

        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(ontologies, sfp, urlScheme);

        CloudHelper<OWLDatatype> helper = new CloudHelper<>(cloudModel, threshold, zoom);
        CloudDoclet<OWLDatatype> cloudDoclet = new CloudDoclet<>(kit, helper);

        model.addAttribute("title", "Datatypes Usage Cloud");
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("content", renderDoclets(request, cloudDoclet));

        return "doclet";
    }
}
