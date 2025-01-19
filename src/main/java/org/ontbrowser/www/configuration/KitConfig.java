package org.ontbrowser.www.configuration;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.impl.OWLHTMLKitImpl;
import org.ontbrowser.www.io.OntologyLoader;
import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.renderer.MOSRenderer;
import org.ontbrowser.www.renderer.OntologyShortFormProvider;
import org.ontbrowser.www.renderer.RendererFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.OWLObjectRenderer;
import org.semanticweb.owlapi.io.ToStringRenderer;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.io.FileNotFoundException;
import java.net.URI;

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
    public OWLHTMLKit owlhtmlKit(@Value("${ontology.root.location}") String root,
                                 @Value("${renderer.annotation.uri}") URI labelURI,
                                 @Value("${renderer.annotation.lang}") String labelLang) throws OWLOntologyCreationException {
        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        OntologyLoader loader = new OntologyLoader();

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
        OWLHTMLKit kit = new OWLHTMLKitImpl(mngr, ont, ontologyShortFormProvider());
        kit.setLabelParams(labelURI, labelLang);
        ToStringRenderer.setRenderer(() -> new OWLObjectRenderer(){

            @Override
            public void setShortFormProvider(ShortFormProvider shortFormProvider) {
                // do nothing
            }

            @Override
            public String render(OWLObject owlObject) {
                return kit.render(owlObject);
            }
        });
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