package org.coode.www.configuration;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.URI;

@Configuration
public class KitConfig {

    @Bean
    public OWLHTMLKit owlhtmlKit(@Value("${ontology.root.location}") String root) throws OWLOntologyCreationException {
        System.err.println("Loading Kit... " + root);
        return new OWLHTMLKitImpl(OWLManager.createOWLOntologyManager(), URI.create(root));
    }
}