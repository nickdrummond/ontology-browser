package org.ontbrowser.www.configuration;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.web.servlet.FilterRegistrationBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.Ordered;

import javax.sql.DataSource;

@Configuration
@ConditionalOnProperty(name = "ontology.backend", havingValue = "db")
public class DbConfig {
    @Bean
    public FilterRegistrationBean<DBConnectionFilter> dbConnectionFilter(DataSource dataSource) {
        var registration = new FilterRegistrationBean<>(new DBConnectionFilter(dataSource));
        registration.addUrlPatterns("/*");
        registration.setOrder(Ordered.HIGHEST_PRECEDENCE);
        return registration;
    }
}


