package org.ontbrowser.www.feature.admin;

import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ModelAttribute;

import java.security.Principal;

/**
 * Controller advice that adds admin to the model if logged in.
 */
@ControllerAdvice
public class AdminControllerAdvice {

    @ModelAttribute("admin")
    public boolean addAdminAttribute() {
        var authentication = SecurityContextHolder.getContext().getAuthentication();
        return authentication != null && isAdmin(authentication);
    }

    private boolean isAdmin(Principal principal) {
        if (principal instanceof UsernamePasswordAuthenticationToken token) {
            var authorities = token.getAuthorities();
            if (!authorities.isEmpty()) {
                return authorities.stream().anyMatch(auth -> auth.getAuthority().equals("ROLE_ADMIN"));
            }
        }
        return false;
    }
}
