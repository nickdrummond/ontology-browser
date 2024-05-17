package org.ontbrowser.www.service;

import org.ontbrowser.www.kit.OWLHTMLKit;

import jakarta.servlet.http.HttpServletRequest;

/**
 * Created by nickdrummond on 26/08/2015.
 */
public interface SessionManager {

    OWLHTMLKit getHTMLKit(HttpServletRequest request);
}
