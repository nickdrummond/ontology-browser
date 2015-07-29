package org.coode.www.controller;

import com.google.common.base.Optional;
import org.coode.html.url.URLScheme;
import org.coode.www.cloud.*;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.SessionAttributes;

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
                                  final Model model) throws OntServerException {

        ClassesByUsageCloud cloudModel = new ClassesByUsageCloud(kit.getOWLServer().getOntologies());
        return cloud(kit, model, "Classes Usage Cloud", cloudModel);
    }

    @RequestMapping(value = "/individuals")
    public String getIndividualsCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                      final Model model) throws OntServerException {

        IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(kit.getOWLServer().getOntologies());
        return cloud(kit, model, "Individuals Usage Cloud", cloudModel);
    }

    @RequestMapping(value = "/objectproperties")
    public String getObjectPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                           final Model model) throws OntServerException {

        ObjectPropsByUsageCloud cloudModel = new ObjectPropsByUsageCloud(kit.getOWLServer().getOntologies());
        return cloud(kit, model, "Object Properties Usage Cloud", cloudModel);
    }

    @RequestMapping(value = "/dataproperties")
    public String getDataPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                         final Model model) throws OntServerException {

        DataPropsByUsageCloud cloudModel = new DataPropsByUsageCloud(kit.getOWLServer().getOntologies());
        return cloud(kit, model, "Data Properties Usage Cloud", cloudModel);
    }

    @RequestMapping(value = "/annotationproperties")
    public String getAnnotationPropertiesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                               final Model model) throws OntServerException {

        AnnotationPropsByUsageCloud cloudModel = new AnnotationPropsByUsageCloud(kit.getOWLServer().getOntologies());
        return cloud(kit, model, "Annotation Properties Usage Cloud", cloudModel);
    }

    @RequestMapping(value = "/datatypes")
    public String getDatatypesCloud(@ModelAttribute("kit") final OWLHTMLKit kit,
                                    final Model model) throws OntServerException {

        DatatypesByUsageCloud cloudModel = new DatatypesByUsageCloud(kit.getOWLServer().getOntologies());
        return cloud(kit, model, "Datatypes Usage Cloud", cloudModel);
    }


    private <T extends OWLEntity>String cloud(final OWLHTMLKit kit,
                                              final Model model,
                                              final String title,
                                              final CloudModel<T> cloudModel) {

        CloudHelper<T> helper = new CloudHelper<T>(cloudModel, threshold, zoom);

        OWLHTMLRenderer owlRenderer = new OWLHTMLRenderer(kit, Optional.absent());

        Set<OWLOntology> ontologies = kit.getOWLServer().getOntologies();

        model.addAttribute("title", title);
        model.addAttribute("options", optionsService.getOptionsAsMap(kit));
        model.addAttribute("activeOntology", kit.getOWLServer().getActiveOntology());
        model.addAttribute("ontologies", ontologies);
        model.addAttribute("cloud", cloudModel);
        model.addAttribute("helper", helper);
        model.addAttribute("mos", owlRenderer);

        return "cloud";
    }
}
