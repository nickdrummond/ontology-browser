package org.ontbrowser.www.configuration;

import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.io.OntologyLoader;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.impl.OWLHTMLKitImpl;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.OntologyShortFormProvider;
import org.ontbrowser.www.renderer.RendererFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.URI;
import java.util.List;

@Configuration
public class KitConfig {

    @Value("${redirect.root}")
    protected String redirectRoot;

    protected final Logger logger = LoggerFactory.getLogger(KitConfig.class);

    @Bean
    public ProjectInfo projectInfo(@Value("${project.name}")    String name,
                                   @Value("${project.contact}") String contact,
                                   @Value("${project.url}") String url,
                                   @Value("${project.tagline}") String tagline,
                                   @Value("${spring.profiles.active:}") String activeProfiles) {
        return new ProjectInfo(name, contact, url, tagline, activeProfiles);
    }

    @Bean
    public OntologyLoader loader() {
        return new OntologyLoader();
    }

    @Bean
    public OWLHTMLKit owlhtmlKit(
            List<BeforeLoad> beforeLoad,
            OntologyLoader loader,
            @Value("${ontology.root.location}") String root,
            @Value("${renderer.annotation.uri}") URI labelURI,
            @Value("${renderer.annotation.lang}") String labelLang
    ) throws OWLOntologyCreationException {

        beforeLoad.forEach(bl -> logger.info("Before load: {}", bl.getClass().getSimpleName()));

        var mngr = OWLManager.createOWLOntologyManager();
        OWLOntology ont;
        if (redirectRoot == null) {
            // Only load the ontologies if we don't have a redirect
            logger.info("Loading {}", root);
            ont = loader.loadOntologies(mngr, root);
        }
        else {
            logger.info("Not loading ontologies as redirect in place");
            ont = mngr.createOntology();
        }
        OWLHTMLKit kit = new OWLHTMLKitImpl(ont, ontologyShortFormProvider());
        kit.setLabelParams(labelURI, labelLang);
        return kit;
    }

    @Bean
    public OntologyShortFormProvider ontologyShortFormProvider() {
        return new OntologyShortFormProvider(OWLRDFVocabulary.RDFS_LABEL.getIRI());
    }

    @Bean
    public RendererFactory rendererFactory(OWLHTMLKit kit) {
        return new RendererFactory(kit.getShortFormProvider(),
                                   kit.getOntologySFP(),
                                   kit.getURLScheme(),
                                   kit.getFinder());
    }
}