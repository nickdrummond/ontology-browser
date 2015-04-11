package org.coode.www.controller;

import org.coode.html.doclet.Doclet;
import org.coode.html.util.ServletUtils;
import org.coode.www.mngr.SessionManager;
import org.coode.www.model.ApplicationInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;

abstract public class ApplicationController {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Autowired
    protected ApplicationInfo applicationInfo;

    @Autowired
    protected SessionManager sessionManager;

    // TODO remove me
    protected String renderDoclets(final HttpServletRequest request, Doclet... doclets) {
        return renderDoclets(ServletUtils.getPageURL(request), doclets);
    }

    // TODO remove me
    protected String renderDoclets(final URL pageUrl, Doclet... doclets) {
        logger.warn("Using deprecated doclets");
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);

        for (Doclet doclet: doclets) {
            doclet.renderAll(pageUrl, writer);
        }
        return stringWriter.toString();
    }
}
