package org.ontbrowser.www.interceptor;

import org.springframework.web.servlet.HandlerInterceptor;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

// TODO use profiles to turn on
public class RedirectInterceptor implements HandlerInterceptor {

    private final String redirectRoot;

    public RedirectInterceptor(String redirectRoot) {
        this.redirectRoot = redirectRoot;
    }

    @Override
    public boolean preHandle(HttpServletRequest request, HttpServletResponse response, Object handler) throws Exception {
        StringBuilder sb = new StringBuilder();
        sb.append(redirectRoot);
        sb.append(request.getRequestURI());
        String qs = request.getQueryString();
        if (qs != null && !qs.isEmpty()) {
            sb.append("?");
            sb.append(qs);
        }
        response.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
        response.setHeader("Location", sb.toString());
        return false;
    }
}
