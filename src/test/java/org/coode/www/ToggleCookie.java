package org.coode.www;

import org.coode.www.OntologyBrowserConstants;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Dec 6, 2010<br><br>
 */
public class ToggleCookie extends HttpServlet{

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
        boolean clearing = false;
        resp.setContentType("text/plain");
        final Cookie[] cookies = req.getCookies();
        if (cookies != null){
            for (Cookie cookie : cookies){
                if (cookie.getName().equals(OntologyBrowserConstants.LABEL_COOKIE_NAME)){
                    resp.getWriter().print("clearing cookie " + cookie.getValue());
                    cookie = new Cookie(OntologyBrowserConstants.LABEL_COOKIE_NAME, "");
                    cookie.setPath(req.getContextPath() + "/");
                    cookie.setMaxAge(0); // until session expires
                    resp.addCookie(cookie);
                    clearing = true;
                }
            }
        }
        if (!clearing){
            Cookie cookie = new Cookie(OntologyBrowserConstants.LABEL_COOKIE_NAME, "test");
            cookie.setPath(req.getContextPath() + "/");
            cookie.setMaxAge(-1); // until session expires
            resp.addCookie(cookie);
            resp.getWriter().print("Setting cookie " + cookie.getValue());
        }

        resp.getWriter().flush();
    }
}
