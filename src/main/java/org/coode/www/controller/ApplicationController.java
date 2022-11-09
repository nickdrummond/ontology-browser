package org.coode.www.controller;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ApplicationInfo;
import org.coode.www.service.OptionsService;
import org.coode.www.service.SessionManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.web.bind.annotation.ModelAttribute;

import javax.servlet.http.HttpServletRequest;

abstract public class ApplicationController {

    protected final Logger logger = LoggerFactory.getLogger(getClass());

    @Value("${redirect.root}")
    protected String redirectRoot;

    @Autowired
    protected ApplicationInfo applicationInfo;

    @Autowired
    protected OWLHTMLKit kit;

    @ModelAttribute("applicationInfo")
    public ApplicationInfo getApplicationInfo() {
        return applicationInfo;
    }

    @ModelAttribute("kit")
    public OWLHTMLKit getKit() { return kit; }

    protected String redirect(HttpServletRequest request) {
        StringBuilder sb = new StringBuilder("redirect:");
        sb.append(redirectRoot);
        sb.append(request.getRequestURI());
        String qs = request.getQueryString();
        if (qs != null && !qs.isEmpty()) {
            sb.append("?");
            sb.append(qs);
        }
        return sb.toString();
    }
}
