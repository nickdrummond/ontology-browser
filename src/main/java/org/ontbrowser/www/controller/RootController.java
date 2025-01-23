package org.ontbrowser.www.controller;

import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.feature.cloud.CloudHelper;
import org.ontbrowser.www.feature.cloud.model.AbstractOWLCloudModel;
import org.ontbrowser.www.feature.cloud.model.ClassesByUsageCloud;
import org.ontbrowser.www.feature.cloud.model.IndividualsByUsageCloud;
import org.ontbrowser.www.feature.cloud.model.ObjectPropsByUsageCloud;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import java.io.IOException;
import java.util.Set;

@RestController
public class RootController extends ApplicationController {

    enum IndexType {
        CLASSES_USAGE_CLOUD, INDIVIDUALS_USAGE_CLOUD, OBJECT_PROPERTIES_USAGE_CLOUD
    }

    @Value("${INDEX_TYPE:CLASSES_USAGE_CLOUD}")
    private IndexType indexType;

    // Entry point
    @GetMapping("/")
    public ModelAndView index(
            final Model model,
            @ModelAttribute OWLOntology ont,
            @RequestParam(required=false) final String redirect,
            HttpServletResponse response) throws IOException {

        if (redirect != null) {
            response.sendRedirect(redirect);
            return new ModelAndView();
        }
        else {
            Set<OWLOntology> ontologies = ont.getImportsClosure();

            AbstractOWLCloudModel cloudModel = switch (indexType) {
                case INDIVIDUALS_USAGE_CLOUD -> new IndividualsByUsageCloud(ontologies);
                case OBJECT_PROPERTIES_USAGE_CLOUD -> new ObjectPropsByUsageCloud(ontologies);
                default -> new ClassesByUsageCloud(ontologies);
            };

            CloudHelper helper = new CloudHelper<>(cloudModel);
            helper.setThreshold(4);
            helper.setZoom(1);
            helper.setNormalise(false);

            String entityType = switch (indexType) {
                case INDIVIDUALS_USAGE_CLOUD -> "individual";
                case OBJECT_PROPERTIES_USAGE_CLOUD -> "object property";
                case CLASSES_USAGE_CLOUD -> "class";
            };

            model.addAttribute("entityType", entityType);
            model.addAttribute("ontologies", ontologies);
            model.addAttribute("cloud", cloudModel);
            model.addAttribute("helper", helper);
            model.addAttribute("mos", rendererFactory.getHTMLRenderer(ont));

            return new ModelAndView("index");
        }
    }
}
