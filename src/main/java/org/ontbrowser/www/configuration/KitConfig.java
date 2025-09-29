package org.ontbrowser.www.configuration;

import org.apache.lucene.store.ByteBuffersDirectory;
import org.apache.lucene.store.Directory;
import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.io.OntologyLoader;
import org.ontbrowser.www.kit.Config;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.ontbrowser.www.kit.impl.OWLHTMLKitInternals;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.RendererFactory;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.context.ApplicationEventPublisher;

import java.net.URI;
import java.util.List;

@Configuration
public class KitConfig {

    @Value("${redirect.root}")
    protected String redirectRoot;

    protected final Logger logger = LoggerFactory.getLogger(KitConfig.class);

    @Bean
    public ProjectInfo projectInfo(@Value("${project.name}") String name,
                                   @Value("${project.contact}") String contact,
                                   @Value("${project.url}") String url,
                                   @Value("${project.tagline}") String tagline,
                                   @Value("${spring.profiles.active:}") String activeProfiles) {
        return new ProjectInfo(name, contact, url, tagline, activeProfiles);
    }

    @Bean
    public Config config(
            @Value("${ontology.root.location}") String root,
            @Value("${renderer.annotation.uri}") URI labelURI,
            @Value("${renderer.annotation.lang}") String labelLang
    ) {
        return new Config(root, IRI.create(labelURI), labelLang);
    }

    @Bean
    public RestartableKit owlhtmlKit(
            List<BeforeLoad> beforeLoad,
            Config config,
            ApplicationEventPublisher eventPublisher
    ) throws OWLOntologyCreationException {

        beforeLoad.forEach(bl -> {
            logger.info("Before load: {}", bl.getClass().getSimpleName());
            bl.beforeLoad(config);
        });
        OWLOntology ont = new OntologyLoader().loadOntologies(config.root());
        var internals = new OWLHTMLKitInternals(ont, config);

        return new RestartableKit(internals, beforeLoad, eventPublisher);
    }

    @Bean
    public RendererFactory rendererFactory(OWLHTMLKit kit) {
        return new RendererFactory(
                kit.getShortFormProvider(),
                kit.getOntologySFP(),
                kit.getIriShortFormProvider(),
                kit.getURLScheme(),
                kit.getFinder()
        );
    }

    @Profile("lucene")
    @Bean
    public Directory luceneDirectory() {
        return new ByteBuffersDirectory();
    }
}