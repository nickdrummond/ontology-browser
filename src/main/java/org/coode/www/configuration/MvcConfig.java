package org.coode.www.configuration;

import org.coode.www.formatter.OntologyIdFormatter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.format.FormatterRegistry;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurerAdapter;

@Configuration
@ComponentScan({"org.coode.www.formatter"})
@EnableWebMvc
@PropertySource("classpath:application.properties")
public class MvcConfig extends WebMvcConfigurerAdapter {

    @Autowired
    private OntologyIdFormatter ontologyIdFormatter;

    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addFormatter(ontologyIdFormatter);
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/static/**").addResourceLocations("/static/");
    }

    @Bean
    public static PropertySourcesPlaceholderConfigurer placeHolderConfigurer() {
        return new PropertySourcesPlaceholderConfigurer();
    }
}