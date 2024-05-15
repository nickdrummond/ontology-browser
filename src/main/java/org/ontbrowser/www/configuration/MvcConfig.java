package org.ontbrowser.www.configuration;

import org.ontbrowser.www.interceptor.CachingInterceptor;
import org.ontbrowser.www.interceptor.RedirectInterceptor;
import org.ontbrowser.www.renderer.OntologyIdFormatter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.*;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.format.FormatterRegistry;
import org.springframework.http.CacheControl;
import org.springframework.web.servlet.config.annotation.EnableWebMvc;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.ResourceHandlerRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;
import org.springframework.web.servlet.mvc.WebContentInterceptor;

import java.util.concurrent.TimeUnit;

@Configuration
@ComponentScan({"org.ontbrowser.www.renderer", "org.ontbrowser.www.interceptor"})
@EnableWebMvc
@PropertySource("classpath:application.properties")
public class MvcConfig implements WebMvcConfigurer {

    private static final Logger log = LoggerFactory.getLogger(MvcConfig.class);

    @Value("${redirect.root}")
    protected String redirectRoot;

    private final OntologyIdFormatter ontologyIdFormatter;

    private final CachingInterceptor cachingInterceptor;

    public MvcConfig(
            @Autowired OntologyIdFormatter ontologyIdFormatter,
            @Autowired CachingInterceptor cachingInterceptor) {
        this.ontologyIdFormatter = ontologyIdFormatter;
        this.cachingInterceptor = cachingInterceptor;
    }

    @Override
    public void addFormatters(FormatterRegistry registry) {
        registry.addFormatter(ontologyIdFormatter);
    }

    @Override
    public void addResourceHandlers(ResourceHandlerRegistry registry) {
        registry.addResourceHandler("/static/**")
                .addResourceLocations("/static/")
                .setCacheControl(CacheControl.maxAge(1, TimeUnit.DAYS));

        registry.addResourceHandler("/robots.txt")
                .addResourceLocations("/static/robots.txt")
                .setCacheControl(CacheControl.maxAge(1, TimeUnit.DAYS));
    }

    @Bean
    public static PropertySourcesPlaceholderConfigurer placeHolderConfigurer() {
        return new PropertySourcesPlaceholderConfigurer();
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        if (redirectRoot != null) {
            log.warn("Redirecting all requests to {}", redirectRoot);
            registry.addInterceptor(new RedirectInterceptor(redirectRoot)).addPathPatterns("/**");
        }
        else {
            log.info("No redirect set");
        }

        if (cachingInterceptor != null) {
            registry.addInterceptor(cachingInterceptor);
        }
    }
}