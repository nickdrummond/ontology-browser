package org.ontbrowser.www.feature.editing;

import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.kit.impl.OWLHTMLKitInternals;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;

import java.util.List;

@Configuration
@Profile("editing")
public class EditableKitConfig {

    private static final Logger log = LoggerFactory.getLogger(EditableKitConfig.class);

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
