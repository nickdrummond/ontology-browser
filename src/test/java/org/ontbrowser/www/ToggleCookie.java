package org.ontbrowser.www;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public class ToggleCookie extends HttpServlet{

    private static final String LABEL_COOKIE_NAME = "label";

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws IOException {
        boolean clearing = false;
        resp.setContentType("text/plain");
        final Cookie[] cookies = req.getCookies();
        if (cookies != null){
            for (Cookie cookie : cookies){
                if (cookie.getName().equals(LABEL_COOKIE_NAME)){
                    resp.getWriter().print("clearing cookie " + cookie.getValue());
                    cookie = new Cookie(LABEL_COOKIE_NAME, "");
                    cookie.setPath(req.getContextPath() + "/");
                    cookie.setMaxAge(0); // until session expires
                    resp.addCookie(cookie);
                    clearing = true;
                }
            }
        }
        if (!clearing){
            Cookie cookie = new Cookie(LABEL_COOKIE_NAME, "test");
            cookie.setPath(req.getContextPath() + "/");
            cookie.setMaxAge(-1); // until session expires
            resp.addCookie(cookie);
            resp.getWriter().print("Setting cookie " + cookie.getValue());
        }

        resp.getWriter().flush();
    }
}
