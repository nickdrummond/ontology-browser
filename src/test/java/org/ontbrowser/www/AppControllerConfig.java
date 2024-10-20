package org.ontbrowser.www;

import org.ontbrowser.www.model.ProjectInfo;
import org.ontbrowser.www.service.SessionManager;
import org.springframework.context.annotation.Bean;

import static org.mockito.Mockito.mock;

public class AppControllerConfig {

    @Bean
    public ProjectInfo applicationInfo() {
        return new ProjectInfo("App name", "contact", "http://example.com/app", "tagline", "");
    }

    @Bean
    protected SessionManager sessionManager() {
        return mock(SessionManager.class);
    }

}
