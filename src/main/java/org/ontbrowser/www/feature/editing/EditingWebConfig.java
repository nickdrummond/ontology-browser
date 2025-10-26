package org.ontbrowser.www.feature.editing;

import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.kit.impl.OWLHTMLKitInternals;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

import java.util.List;

@Profile("editing")
@Configuration
@ControllerAdvice
public class EditingWebConfig implements WebMvcConfigurer {

    private static final Logger log = LoggerFactory.getLogger(EditingWebConfig.class);
    private final EditModeInterceptor editModeInterceptor;

    @Autowired
    public EditingWebConfig(EditModeInterceptor editModeInterceptor) {
        this.editModeInterceptor = editModeInterceptor;
    }

    @Override
    public void addInterceptors(InterceptorRegistry registry) {
        registry.addInterceptor(editModeInterceptor);
    }

    @Bean
    @Primary
    public RestartableKit editableKit(
            OWLHTMLKitInternals internals,
            List<BeforeLoad> beforeLoad,
            ApplicationEventPublisher eventPublisher
    ) {
        log.info("EDIT MODE");
        return new EditableKit(internals, beforeLoad, eventPublisher);
    }
}
