package org.coode.www.controller;

import org.coode.html.url.URLScheme;
import org.coode.www.cloud.IndividualsByUsageCloud;
import org.coode.www.exception.NotAuthorisedException;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.XmlUrl;
import org.coode.www.model.XmlUrlSet;
import org.coode.www.renderer.OWLHTMLRenderer;
import org.coode.www.service.CloudHelper;
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
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.XmlWebApplicationContext;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.annotation.Nullable;
import javax.servlet.http.HttpServletRequest;
import java.util.Objects;
import java.util.Set;
import java.util.function.Consumer;

@Controller
public class RootController extends ApplicationController implements ApplicationContextAware {

    @Nullable
    @Value("${restart.secret}")
    private String restartSecret;

    private XmlWebApplicationContext applicationContext;

    // Entry point
    @GetMapping("/")
    public String index(final Model model,
                        @RequestParam(required=false) final String redirect) {

        if (redirect != null) {
            return "redirect:" + redirect;
        }
        else {
            Set<OWLOntology> ontologies = kit.getOntologies();

            IndividualsByUsageCloud cloudModel = new IndividualsByUsageCloud(ontologies);

            CloudHelper<OWLNamedIndividual> helper = new CloudHelper<>(cloudModel);
            helper.setThreshold(14);
            helper.setZoom(4);
            helper.setNormalise(true);

            OWLOntology ont = kit.getActiveOntology();

            model.addAttribute("activeOntology", ont);
            model.addAttribute("ontologies", ontologies);
            model.addAttribute("cloud", cloudModel);
            model.addAttribute("helper", helper);
            model.addAttribute("mos", rendererFactory.getRenderer(ont));

            return "index";
        }
    }

    @GetMapping("/restart")
    public String reload(
            @RequestParam(required=false) final String redirect,
            @RequestParam final String secret) throws NotAuthorisedException {

        if (!Objects.equals(secret, restartSecret)) {
            throw new NotAuthorisedException();
        }

        // Shouldn't do this - https://www.baeldung.com/java-restart-spring-boot-app
        applicationContext.refresh();

        if (redirect != null) {
            return "redirect:" + redirect;
        }

        return "redirect:/";
    }

    @GetMapping(value = "/sitemap.xml")
    @ResponseBody
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

        OWLOntology activeOntology = kit.getActiveOntology();
        URLScheme scheme = kit.getURLScheme();

        // ontologies
        activeOntology.getImportsClosure().forEach(o -> xmlUrlSet.addUrl(new XmlUrl(baseUrl + scheme.getURLForOWLObject(o))));

        // entities
        Consumer<OWLEntity> action = i -> xmlUrlSet.addUrl(new XmlUrl(baseUrl + scheme.getURLForOWLObject(i)));
        activeOntology.getClassesInSignature(Imports.INCLUDED).forEach(action);
        activeOntology.getIndividualsInSignature(Imports.INCLUDED).forEach(action);
        activeOntology.getObjectPropertiesInSignature(Imports.INCLUDED).forEach(action);
        activeOntology.getDataPropertiesInSignature(Imports.INCLUDED).forEach(action);
        activeOntology.getAnnotationPropertiesInSignature(Imports.INCLUDED).forEach(action);
        activeOntology.getDatatypesInSignature(Imports.INCLUDED).forEach(action);

        return xmlUrlSet;
    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = (XmlWebApplicationContext)applicationContext;
    }
}
