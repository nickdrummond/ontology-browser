package org.coode.www;

import org.coode.www.model.ApplicationInfo;
import org.coode.www.service.SessionManager;
import org.springframework.context.annotation.Bean;

import static org.mockito.Mockito.mock;

public class AppControllerConfig {

    @Bean
    public ApplicationInfo applicationInfo() {
        return new ApplicationInfo("App name", "App version", "http://example.com/app");
    }

    @Bean
    protected SessionManager sessionManager() {
        return mock(SessionManager.class);
    }

}
