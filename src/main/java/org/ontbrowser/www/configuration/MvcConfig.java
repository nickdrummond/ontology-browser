package org.ontbrowser.www.configuration;

import org.ontbrowser.www.interceptor.CachingInterceptor;
import org.ontbrowser.www.interceptor.RedirectInterceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

@Configuration
@EnableScheduling
public class MvcConfig implements WebMvcConfigurer {

    private static final Logger log = LoggerFactory.getLogger(MvcConfig.class);

    @Value("${redirect.root}")
    protected String redirectRoot;

    private final CachingInterceptor cachingInterceptor;

    public MvcConfig(
            @Autowired(required = false) CachingInterceptor cachingInterceptor) {
        this.cachingInterceptor = cachingInterceptor;
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