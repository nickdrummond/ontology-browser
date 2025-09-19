package org.ontbrowser.www.interceptor;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ontbrowser.www.model.ProjectInfo;
import org.springframework.stereotype.Component;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.ModelAndView;

import javax.annotation.Nonnull;

/**
 * When the front-end requests page fragments, it needs to know the title of the page.
 * So, we add a "title" header to the response, if the model contains a "title" attribute.
 */
@Component
public class TitleHeaderInterceptor implements HandlerInterceptor {

    private final ProjectInfo projectInfo;

    public TitleHeaderInterceptor(ProjectInfo projectInfo) {
        this.projectInfo = projectInfo;
    }

    @Override
    public void postHandle(
            @Nonnull HttpServletRequest request,
            @Nonnull HttpServletResponse response,
            @Nonnull Object handler,
            ModelAndView modelAndView
    ) {
        if (modelAndView != null) {
            Object titleAttr = modelAndView.getModel().get("title");
            if (titleAttr != null) {
                String title = titleAttr.toString();
                response.addHeader("title", projectInfo.name() + ": " + title);
            }
        }
    }
}