package org.ontbrowser.www;

import org.ontbrowser.www.model.ProjectInfo;
import org.springframework.context.annotation.Bean;

public class AppControllerConfig {

    @Bean
    public ProjectInfo applicationInfo() {
        return new ProjectInfo("App name", "contact", "http://example.com/app", "tagline", "");
    }
}
