package org.ontbrowser.www.configuration;

import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.store.Directory;
import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.backend.memory.kit.impl.OWLHTMLKitInternals;
import org.ontbrowser.www.backend.memory.kit.impl.RestartableKit;
import org.ontbrowser.www.io.OntologyLoader;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;

import java.util.List;

import static org.ontbrowser.www.backend.memory.kit.impl.OWLHTMLKitInternals.readOnlyKit;

@Configuration
@ConditionalOnProperty(name = "ontology.backend", havingValue = "memory", matchIfMissing = true)
public class KitConfig {

    @Value("${redirect.root}")
    protected String redirectRoot;

    protected final Logger log = LoggerFactory.getLogger(KitConfig.class);

    @Bean
    public OWLHTMLKitInternals internals(
            List<BeforeLoad> beforeLoad,
            Config config
    ) throws OWLOntologyCreationException {
        beforeLoad.forEach(bl -> {
            log.info("Before load: {}", bl.getClass().getSimpleName());
            bl.beforeLoad(config);
        });
        var ont = new OntologyLoader().loadOntologies(config.root());
        return readOnlyKit(ont, config);
    }

    @Profile("!editing")
    @Primary
    @Bean
    public RestartableKit owlHtmlKit(
            OWLHTMLKitInternals internals,
            List<BeforeLoad> beforeLoad,
            ApplicationEventPublisher eventPublisher
    ) {
        log.info("READ ONLY MODE");
        return new RestartableKit(internals, beforeLoad, eventPublisher);
    }

    @Profile("lucene")
    @Bean
    public Directory luceneDirectory() {
        return new ByteBuffersDirectory();
    }
}