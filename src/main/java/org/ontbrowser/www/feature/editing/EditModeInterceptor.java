package org.ontbrowser.www.feature.editing;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

/**
 * Interceptor to set edit mode based on request header.
 */
@Profile("editing")
@Component
public class EditModeInterceptor implements HandlerInterceptor {
    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        // Check header to determine edit mode
        boolean isEditMode = "true".equals(request.getHeader("X-Edit-Mode"));
        EditModeContext.setEditMode(isEditMode);
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                                Object handler, Exception ex) {
        EditModeContext.clear();
    }
}