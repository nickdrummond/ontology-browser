package org.coode.www;

import org.coode.www.model.ProjectInfo;
import org.coode.www.service.SessionManager;
import org.springframework.context.annotation.Bean;

import static org.mockito.Mockito.mock;

public class AppControllerConfig {

    @Bean
    public ProjectInfo applicationInfo() {
        return new ProjectInfo("App name", "contact", "http://example.com/app", "tagline");
    }

    @Bean
    protected SessionManager sessionManager() {
        return mock(SessionManager.class);
    }

}
