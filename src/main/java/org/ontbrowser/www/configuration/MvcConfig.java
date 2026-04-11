package org.ontbrowser.www.configuration;

import org.ontbrowser.www.interceptor.CachingInterceptor;
import org.ontbrowser.www.interceptor.LegacyEntityIdInterceptor;
import org.ontbrowser.www.interceptor.RedirectInterceptor;
import org.ontbrowser.www.interceptor.TitleHeaderInterceptor;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import javax.annotation.Nonnull;

@Configuration
@EnableScheduling
public class MvcConfig implements WebMvcConfigurer {

    private static final Logger log = LoggerFactory.getLogger(MvcConfig.class);

    private final TitleHeaderInterceptor titleHeaderInterceptor;
    private final LegacyEntityIdInterceptor legacyEntityIdInterceptor;

    @Value("${redirect.root}")
    protected String redirectRoot;

    private final CachingInterceptor cachingInterceptor;

    public MvcConfig(
            @Nonnull TitleHeaderInterceptor titleHeaderInterceptor,
            @Nonnull LegacyEntityIdInterceptor legacyEntityIdInterceptor,
            @Autowired(required = false) CachingInterceptor cachingInterceptor
    ) {
        this.titleHeaderInterceptor = titleHeaderInterceptor;
        this.legacyEntityIdInterceptor = legacyEntityIdInterceptor;
        this.cachingInterceptor = cachingInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(titleHeaderInterceptor);
        registry.addInterceptor(legacyEntityIdInterceptor);

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