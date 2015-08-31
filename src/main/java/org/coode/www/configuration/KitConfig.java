package org.coode.www.configuration;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
public class KitConfig {

    @Bean
    @Scope("prototype")
    public OWLHTMLKit owlhtmlKit(@Value("${ontology.root.iri}") String root) {
        return new OWLHTMLKitImpl(OWLManager.createOWLOntologyManager(), IRI.create(root));
    }
}