package org.coode.www.configuration;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.coode.www.kit.impl.OntologyLoader;
import org.coode.www.renderer.RendererFactory;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.URI;

@Configuration
public class KitConfig {

    protected final Logger logger = LoggerFactory.getLogger(KitConfig.class);

    @Bean
    public OWLHTMLKit owlhtmlKit(@Value("${ontology.root.location}")   URI root,
                                 @Value("${renderer.annotation.uri}")  URI labelURI,
                                 @Value("${renderer.annotation.lang}") String labelLang) throws OWLOntologyCreationException {
        logger.info("Loading Kit... " + root);
        OntologyLoader loader = new OntologyLoader();
        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        OWLOntology ont = loader.loadOntologies(mngr, root);
        OWLHTMLKit kit = new OWLHTMLKitImpl(mngr, ont);
        kit.setLabelParams(labelURI, labelLang);
        return kit;
    }

    @Bean
    public RendererFactory rendererFactory(OWLHTMLKit kit) {
        return new RendererFactory(kit.getShortFormProvider(),
                                   kit.getOntologyShortFormProvider(),
                                   kit.getURLScheme(),
                                   kit.getFinder());
    }
}