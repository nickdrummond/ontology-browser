package org.ontbrowser.www.controller;

import org.ontbrowser.www.model.cloud.AbstractOWLCloudModel;
import org.ontbrowser.www.model.cloud.ClassesByUsageCloud;
import org.ontbrowser.www.model.cloud.ObjectPropsByUsageCloud;
import org.ontbrowser.www.url.URLScheme;
import org.ontbrowser.www.model.cloud.IndividualsByUsageCloud;
import org.ontbrowser.www.exception.NotAuthorisedException;
import org.ontbrowser.www.model.XmlUrl;
import org.ontbrowser.www.model.XmlUrlSet;
import org.ontbrowser.www.service.CloudHelper;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.support.XmlWebApplicationContext;
import org.springframework.web.servlet.ModelAndView;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.annotation.Nullable;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

@RestController
public class RootController extends ApplicationController implements ApplicationContextAware {

    enum IndexType {
        CLASSES_USAGE_CLOUD, INDIVIDUALS_USAGE_CLOUD, OBJECT_PROPERTIES_USAGE_CLOUD
    }

    @Nullable
    @Value("${restart.secret}")
    private String restartSecret;

    @Value("${INDEX_TYPE:CLASSES_USAGE_CLOUD}")
    private IndexType indexType;

    private XmlWebApplicationContext applicationContext;

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
            model.addAttribute("mos", rendererFactory.getRenderer(ont));

            return new ModelAndView("index");
        }
    }

    @GetMapping("/restart")
    public void reload(
            @RequestParam(required=false) final String redirect,
            @RequestParam final String secret,
            HttpServletResponse response) throws NotAuthorisedException, IOException {

        if (!Objects.equals(secret, restartSecret)) {
            throw new NotAuthorisedException();
        }

        // Shouldn't do this - https://www.baeldung.com/java-restart-spring-boot-app
        applicationContext.refresh();

        response.sendRedirect((redirect != null) ? redirect : "/");
    }

    @GetMapping(value = "/sitemap.xml")
    public XmlUrlSet sitemap() {
        final String baseUrl = ServletUriComponentsBuilder.fromCurrentContextPath().build().toUriString();

        XmlUrlSet xmlUrlSet = new XmlUrlSet();

        // base
        xmlUrlSet.addUrl(new XmlUrl(baseUrl));

        // indices
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/ontologies"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/classes"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/individuals"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/objectproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/dataproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/annotationproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/datatypes"));

        // additional pages
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/dlquery/"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/classes"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/individuals"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/objectproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/dataproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/annotationproperties"));
        xmlUrlSet.addUrl(new XmlUrl(baseUrl + "/clouds/datatypes"));

        OWLOntology ont = kit.getRootOntology();
        URLScheme scheme = kit.getURLScheme();

        // ontologies
        ont.getImportsClosure().forEach(o -> xmlUrlSet.addUrl(new XmlUrl(baseUrl + scheme.getURLForOWLObject(o))));

        // entities
        Consumer<OWLEntity> action = i -> xmlUrlSet.addUrl(new XmlUrl(baseUrl + scheme.getURLForOWLObject(i)));
        ont.getClassesInSignature(Imports.INCLUDED).forEach(action);
        ont.getIndividualsInSignature(Imports.INCLUDED).forEach(action);
        ont.getObjectPropertiesInSignature(Imports.INCLUDED).forEach(action);
        ont.getDataPropertiesInSignature(Imports.INCLUDED).forEach(action);
        ont.getAnnotationPropertiesInSignature(Imports.INCLUDED).forEach(action);
        ont.getDatatypesInSignature(Imports.INCLUDED).forEach(action);

        return xmlUrlSet;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = (XmlWebApplicationContext)applicationContext;
    }
}
