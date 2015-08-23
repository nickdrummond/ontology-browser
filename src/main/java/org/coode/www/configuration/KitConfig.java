package org.coode.www.configuration;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLKitImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;

@Configuration
public class KitConfig {

    @Bean
    @Scope("prototype")
    public OWLHTMLKit owlhtmlKit() {
        return new OWLHTMLKitImpl(OWLManager.createOWLOntologyManager());
    }
}