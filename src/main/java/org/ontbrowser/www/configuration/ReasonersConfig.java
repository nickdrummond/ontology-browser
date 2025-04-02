package org.ontbrowser.www.configuration;

import org.apache.commons.collections4.map.LRUMap;
import org.ontbrowser.www.feature.dlquery.DLQuery;
import org.ontbrowser.www.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.reasoner.ReasonerMomento;
import org.semanticweb.owlapi.model.OWLEntity;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

@Configuration
@Profile("reasoners")
public class ReasonersConfig {

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
    public ReasonerFactoryService reasonerFactoryService(List<ReasonerMomento> reasoners) {
        return new ReasonerFactoryService(reasoners, openlletLabel, structuralLabel);
    }

    @Bean
    public ExecutorService reasonerThreadPool(@Value("${reasoning.threads}") int reasoningThreads) {
        return Executors.newFixedThreadPool(reasoningThreads);
    }

    @Bean
    public Map<DLQuery, Future<Set<OWLEntity>>> reasonerCache(@Value("${reasoning.cache.count}") int cacheCount) {
        return Collections.synchronizedMap(new LRUMap<>(cacheCount));
    }
}