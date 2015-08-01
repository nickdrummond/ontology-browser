package org.coode.www.configuration;

import org.coode.www.model.ApplicationInfo;
import org.coode.www.model.Bookmarks;
import org.coode.www.service.ReasonerFactoryService;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.ClassPathResource;
import org.springframework.web.servlet.view.InternalResourceViewResolver;
import org.springframework.web.servlet.view.JstlView;

import java.util.Arrays;
import java.util.List;

@Configuration
@ComponentScan({"org.coode.www.service", "org.coode.www.repository", "org.coode.www.controller"})
public class ApplicationConfig {

    @Bean
    public InternalResourceViewResolver setupViewResolver() {
        InternalResourceViewResolver resolver = new InternalResourceViewResolver();
        resolver.setPrefix("/WEB-INF/views/");
        resolver.setSuffix(".jsp");
        resolver.setViewClass(JstlView.class);
        return resolver;
    }

    @Bean
    public ApplicationInfo applicationInfo(@Value("${application.name}") String applicationName,
                                           @Value("${application.version}") String applicationVersion,
                                           @Value("${application.url}") String applicationUrl) {
        return new ApplicationInfo(applicationName, applicationVersion, applicationUrl);
    }

    @Bean
    public OntologyIRIShortFormProvider ontologyIRIShortFormProvider() {
        return new OntologyIRIShortFormProvider();
    }

    @Bean
    public Bookmarks bookmarks(@Value("${bookmarks.source}") String bookmarksSource) {
        return new Bookmarks(new ClassPathResource(bookmarksSource));
    }

    @Bean
    public ReasonerFactoryService reasonerFactoryService() {
        // TODO Springify the factory class names?
        List<String> reasonerFactoryNames = Arrays.asList(
                "org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory",
                "uk.ac.manchester.cs.jfact.JFactFactory",
                "org.semanticweb.HermiT.Reasoner$ReasonerFactory",
                "org.semanticweb.owlapi.owllink.OWLlinkHTTPXMLReasonerFactory",
                "uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory",
                "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory"
        );
        return new ReasonerFactoryService(reasonerFactoryNames);
    }
}