package org.ontbrowser.www.configuration;

import org.ontbrowser.www.model.ProjectInfo;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

@Configuration
@ComponentScan({"org.ontbrowser.www.service", "org.ontbrowser.www.controller"})
public class ApplicationConfig {

    @Bean
    public ProjectInfo projectInfo(@Value("${project.name}")    String name,
                                   @Value("${project.contact}") String contact,
                                   @Value("${project.url}") String url,
                                   @Value("${project.tagline}") String tagline,
                                   @Value("${spring.profiles.active:}") String activeProfiles) {
        return new ProjectInfo(name, contact, url, tagline, activeProfiles);
    }
}