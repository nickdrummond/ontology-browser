package org.coode.www.controller;

import org.coode.html.doclet.Doclet;
import org.coode.www.kit.impl.OWLHTMLParam;
import org.coode.www.mngr.SessionManager;
import org.coode.www.model.ApplicationInfo;
import org.coode.www.service.OptionsService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletRequest;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;

abstract public class ApplicationController {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Autowired
    protected ApplicationInfo applicationInfo;

    @Autowired
    protected SessionManager sessionManager;

    @Autowired
    protected OptionsService optionsService;

    @ModelAttribute("applicationInfo")
    public ApplicationInfo getApplicationInfo() {
        return applicationInfo;
    }

    // TODO remove me
    @Deprecated
    protected String renderDoclets(final HttpServletRequest request, Doclet... doclets) {
        try {
            // if the request is for an html-frag and we have a referer page we return the url of the referer
            String query = request.getQueryString();
            StringBuilder requestURL = new StringBuilder(request.getRequestURL().toString());
            boolean appendedParams = false;

            if (query != null){
                for (String param : query.split("&")){
                    if (!param.startsWith(OWLHTMLParam.label.name())){
                        if (appendedParams){
                            requestURL.append("&");
                        }
                        else{
                            requestURL.append("?");
                            appendedParams = true;
                        }
                        requestURL.append(param);
                    }
                }
            }
            URL pageUrl = new URL(requestURL.toString());
            return renderDoclets(pageUrl, doclets);

        } catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }

    // TODO remove me
    @Deprecated
    protected String renderDoclets(final URL pageUrl, Doclet... doclets) {
        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);

        for (Doclet doclet: doclets) {
            doclet.renderAll(pageUrl, writer);
        }
        return stringWriter.toString();
    }
}
