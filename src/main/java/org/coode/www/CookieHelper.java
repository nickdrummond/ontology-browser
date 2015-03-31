package org.coode.www;

import org.slf4j.LoggerFactory; import org.slf4j.Logger;
import org.coode.www.kit.OWLHTMLKit;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class CookieHelper {

    private static final Logger logger = LoggerFactory.getLogger(CookieHelper.class);

    private final String cookieName;
    private final boolean enabled;

    public CookieHelper(String cookieName, boolean enabled) {
        this.cookieName = cookieName;
        this.enabled = enabled;
    }

    public void updateCookies(HttpServletRequest request, HttpServletResponse response, OWLHTMLKit kit) {
        if (enabled) {
            if (kit.isActive() && kit.getCurrentLabel() != null) {
                Cookie cookie = new Cookie(cookieName, kit.getCurrentLabel());
                cookie.setPath(request.getContextPath() + "/");
                cookie.setMaxAge(-1); // until session expires
                response.addCookie(cookie);
                logger.info("Setting cookie " + kit.getCurrentLabel());
            }
            else {
                final Cookie[] cookies = request.getCookies();
                if (cookies != null) {
                    for (Cookie cookie : cookies) {
                        if (cookie.getName().equals(cookieName)) {
                            logger.info("Clearing cookie " + cookie.getValue());
                            cookie = new Cookie(cookieName, "");
                            cookie.setPath(request.getContextPath() + "/");
                            cookie.setMaxAge(0);
                            response.addCookie(cookie);
                        }
                    }
                }
            }
        }
    }

    public String getLabelFromCookie(HttpServletRequest request) {
        if (enabled){
            Cookie[] cookies = request.getCookies();
            if (cookies != null){
                for (Cookie cookie : cookies){
                    if (cookie.getName().equals(cookieName)){
                        logger.info("recovering session from cookie: " + cookie.getValue());
                        return cookie.getValue();
                    }
                }
            }
        }
        return null;
    }
}
