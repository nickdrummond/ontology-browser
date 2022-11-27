package org.coode.www.service;

import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;

import javax.servlet.http.HttpServletRequest;

/**
 * Created by nickdrummond on 26/08/2015.
 */
public interface SessionManager {

    OWLHTMLKit getHTMLKit(HttpServletRequest request) throws OntServerException;
}
