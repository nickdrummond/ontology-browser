package org.coode.www.configuration;

import org.apache.commons.collections4.map.LRUMap;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ProjectInfo;
import org.coode.www.model.ReasonerMomento;
import org.coode.www.service.ReasonerFactoryService;
import org.coode.www.service.ReasonerService;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

@Configuration
@ComponentScan({"org.coode.www.service", "org.coode.www.controller"})
public class ApplicationConfig {

    @Bean
    public ProjectInfo projectInfo(@Value("${project.name}")    String name,
                                   @Value("${project.contact}") String contact,
                                   @Value("${project.url}")     String url,
                                   @Value("${project.tagline}") String tagline) {
        return new ProjectInfo(name, contact, url, tagline);
    }

    @Bean
    public OntologyIRIShortFormProvider ontologyIRIShortFormProvider() {
        return new OntologyIRIShortFormProvider();
    }

    @Value("${reasoner.structural.label}")
    private String structuralLabel;

    @Value("${reasoner.openllet.label}")
    private String openlletLabel;

    @Bean
    public ReasonerMomento structural(@Value("${reasoner.structural.cls}") String cls) {
        return new ReasonerMomento(structuralLabel, cls);
    }

    @Bean
    public ReasonerMomento openllet(@Value("${reasoner.openllet.cls}") String cls) {
        return new ReasonerMomento(openlletLabel, cls);
    }

    @Bean
    public ReasonerMomento jfact(@Value("${reasoner.jfact.cls}")   String cls,
                                 @Value("${reasoner.jfact.label}") String label) {
        return new ReasonerMomento(label, cls);
    }

    @Bean
    public ReasonerMomento hermit(@Value("${reasoner.hermit.cls}")   String cls,
                                  @Value("${reasoner.hermit.label}") String label) {
        return new ReasonerMomento(label, cls);
    }

    @Bean
    public ReasonerMomento owlLink(@Value("${reasoner.owllink.cls}")   String cls,
                                   @Value("${reasoner.owllink.label}") String label) {
        return new ReasonerMomento(label, cls);
    }

    @Bean
    public ReasonerMomento factpp(@Value("${reasoner.factpp.cls}")   String cls,
                                  @Value("${reasoner.factpp.label}") String label) {
        return new ReasonerMomento(label, cls);
    }

    @Bean
    public ReasonerMomento pellet(@Value("${reasoner.pellet.cls}")   String cls,
                                  @Value("${reasoner.pellet.label}") String label) {
        return new ReasonerMomento(label, cls);
    }

    @Bean
    @Autowired
    public ReasonerFactoryService reasonerFactoryService(List<ReasonerMomento> reasoners) {
        return new ReasonerFactoryService(reasoners, openlletLabel, structuralLabel);
    }

    @Bean
    public ExecutorService reasonerThreadPool(@Value("${reasoning.threads}") int reasoningThreads) {
        return Executors.newFixedThreadPool(reasoningThreads);
    }

    @Bean
    public ReasonerService reasonerService(@Value("${reasoning.cache.count}") int cacheCount,
                                           ReasonerFactoryService reasonerFactoryService,
                                           OWLHTMLKit kit,
                                           ExecutorService reasonerThreadPool) {
        return new ReasonerService(kit, reasonerThreadPool, Collections.synchronizedMap(new LRUMap<>(cacheCount)), reasonerFactoryService);
    }
}