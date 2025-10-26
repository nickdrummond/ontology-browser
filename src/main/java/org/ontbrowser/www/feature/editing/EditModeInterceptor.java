package org.ontbrowser.www.feature.editing;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;

/**
 * Interceptor to set edit mode based on session attribute.
 */
@Profile("editing")
@Component
public class EditModeInterceptor implements HandlerInterceptor {
    private static final Logger log = LoggerFactory.getLogger(EditModeInterceptor.class);

    private static final String EDIT_MODE_SESSION_ATTR = "editMode";

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) {
        // Check session attribute to determine edit mode
        Object attr = request.getSession(false) != null
                ? request.getSession(false).getAttribute(EDIT_MODE_SESSION_ATTR)
                : null;
        boolean isEditMode = Boolean.TRUE.equals(attr);
        EditModeContext.setEditMode(isEditMode);
        return true;
    }

    @Override
    public void afterCompletion(HttpServletRequest request, HttpServletResponse response,
                                Object handler, Exception ex) {
        EditModeContext.clear();
    }
}