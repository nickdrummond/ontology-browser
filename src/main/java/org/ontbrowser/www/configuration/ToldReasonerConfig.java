package org.ontbrowser.www.configuration;

import org.apache.commons.collections4.map.LRUMap;
import org.ontbrowser.www.feature.dlquery.DLQuery;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.feature.reasoner.ReasonerMomento;
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
@Profile("!reasoners")
public class ToldReasonerConfig {

    @Value("${reasoner.structural.label}")
    private String structuralLabel;

    @Bean
    public ReasonerMomento structural(@Value("${reasoner.structural.cls}") String cls) {
        return new ReasonerMomento(structuralLabel, cls);
    }

    @Bean
    public ReasonerFactoryService reasonerFactoryService(List<ReasonerMomento> reasoners) {
        // Use Structural always
        return new ReasonerFactoryService(reasoners, structuralLabel, structuralLabel);
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