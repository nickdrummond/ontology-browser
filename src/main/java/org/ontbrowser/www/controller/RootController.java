package org.ontbrowser.www.controller;

import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.exception.NotAuthorisedException;
import org.ontbrowser.www.feature.cloud.CloudHelper;
import org.ontbrowser.www.feature.cloud.model.AbstractOWLCloudModel;
import org.ontbrowser.www.feature.cloud.model.ClassesByUsageCloud;
import org.ontbrowser.www.feature.cloud.model.IndividualsByUsageCloud;
import org.ontbrowser.www.feature.cloud.model.ObjectPropsByUsageCloud;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.context.support.XmlWebApplicationContext;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nullable;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;

@RestController
public class RootController extends ApplicationController { //} implements ApplicationContextAware {

    enum IndexType {
        CLASSES_USAGE_CLOUD, INDIVIDUALS_USAGE_CLOUD, OBJECT_PROPERTIES_USAGE_CLOUD
    }

    @Nullable
    @Value("${restart.secret}")
    private String restartSecret;

    @Value("${INDEX_TYPE:CLASSES_USAGE_CLOUD}")
    private IndexType indexType;

//    private XmlWebApplicationContext applicationContext;

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

//    @GetMapping("/restart")
//    public void reload(
//            @RequestParam(required=false) final String redirect,
//            @RequestParam final String secret,
//            HttpServletResponse response) throws NotAuthorisedException, IOException {
//
//        if (!Objects.equals(secret, restartSecret)) {
//            throw new NotAuthorisedException();
//        }
//
//        // Shouldn't do this - https://www.baeldung.com/java-restart-spring-boot-app
//        applicationContext.refresh();
//
//        response.sendRedirect((redirect != null) ? redirect : "/");
//    }

//    @Override
//    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
//        this.applicationContext = (XmlWebApplicationContext)applicationContext;
//    }
}
