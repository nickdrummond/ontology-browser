package org.ontbrowser.www.configuration;

import org.ontbrowser.www.model.ProjectInfo;
import org.semanticweb.owlapi.model.IRI;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.URI;

@Configuration
public class AppConfig {
    
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
}
