package org.ontbrowser.www.feature.admin;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.AuthenticationSuccessHandler;
import org.springframework.security.web.authentication.SavedRequestAwareAuthenticationSuccessHandler;
import org.springframework.security.web.authentication.logout.LogoutSuccessHandler;
import org.springframework.security.web.authentication.logout.SimpleUrlLogoutSuccessHandler;

import static org.springframework.security.config.Customizer.withDefaults;

// see https://spring.io/blog/2022/02/21/spring-security-without-the-websecurityconfigureradapter
@Configuration
@EnableWebSecurity
public class SecurityConfig {

    @Profile("admin")
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .authorizeHttpRequests(authz -> authz
                        .requestMatchers("/admin/**").authenticated()
                        .requestMatchers("/git/update").authenticated()
                        .anyRequest().permitAll()
                )
                .httpBasic(withDefaults())
                .formLogin(form -> form
                        .loginPage("/login")
                        .successHandler(customLoginSuccessHandler())
                        .permitAll())
                .logout(logout -> logout
                        .logoutUrl("/logout")
                        .logoutSuccessHandler(refererLogoutSuccessHandler())
                );
        return http.build();
    }

    @Bean
    public AuthenticationSuccessHandler customLoginSuccessHandler() {
        return new AuthenticationSuccessHandler() {
            private final SavedRequestAwareAuthenticationSuccessHandler defaultHandler = new SavedRequestAwareAuthenticationSuccessHandler();
            @Override
            public void onAuthenticationSuccess(HttpServletRequest request, HttpServletResponse response, Authentication authentication) throws java.io.IOException, jakarta.servlet.ServletException {
                // Check for session-based loginRedirect first
                Object loginRedirect = request.getSession().getAttribute("loginRedirect");
                if (loginRedirect != null && loginRedirect instanceof String) {
                    String redirectUrl = (String) loginRedirect;
                    request.getSession().removeAttribute("loginRedirect");
                    if (!redirectUrl.isBlank() && !redirectUrl.contains("/login")) {
                        response.sendRedirect(redirectUrl);
                        return;
                    }
                }
                // Fallback to default handler (saved request or /)
                defaultHandler.onAuthenticationSuccess(request, response, authentication);
            }
        };
    }

    @Bean
    public LogoutSuccessHandler refererLogoutSuccessHandler() {
        var handler = new SimpleUrlLogoutSuccessHandler();
        handler.setUseReferer(true); // stay on the same page after logout
        return handler;
    }

    @Profile("!admin")
    @Bean
    public SecurityFilterChain filterChainNoAdmin(HttpSecurity http) throws Exception {
        http
                .authorizeHttpRequests(authz -> authz
                        .anyRequest().permitAll()
                )
                .httpBasic(withDefaults());
        return http.build();
    }

    @Profile("admin")
    @Bean
    public UserDetailsService userDetailsService(
            @Value("${admin.username}") String username,
            @Value("${admin.password}") String password) {
        UserDetails user = User.withDefaultPasswordEncoder()
                .username(username)
                .password(password)
                .roles("ADMIN")
                .build();
        return new InMemoryUserDetailsManager(user);
    }
}
