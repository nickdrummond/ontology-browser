/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.url;

import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLParam;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 17, 2008<br><br>
 *
 * Wraps a give URLScheme, but provides an additional parameter for the permalink
 */
public class PermalinkURLScheme implements URLScheme {

    private static final Logger logger = LoggerFactory.getLogger(PermalinkURLScheme.class);

    private URLScheme baseScheme;


    public PermalinkURLScheme(URLScheme baseScheme) {
        this.baseScheme = baseScheme;
    }

    private URL append(URL url) {
        if (getOWLHTMLKit().getCurrentLabel() != null){
            String link = url.toString();
            if (!link.contains("?")){
                link += "?";
            }
            else{
                link += "&";
            }
            link += OWLHTMLParam.label + "=" + getOWLHTMLKit().getCurrentLabel();

            try {
                return new URL(link);
            }
            catch (MalformedURLException e) {
                logger.error("Cannot create permalink URL", e);
            }
        }
        return url;
    }

    public URL getURLForOWLObject(OWLObject entity) {
        return append(baseScheme.getURLForOWLObject(entity));
    }

    public OWLHTMLKit getOWLHTMLKit() {
        return baseScheme.getOWLHTMLKit();
    }

    public NamedObjectType getType(URL url) {
        return baseScheme.getType(url);
    }

    public URL getBaseURL() {
        return baseScheme.getBaseURL();
    }

    public URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type) {
        return append(baseScheme.getURLForOntologyIndex(ont, type));
    }

    public void setAdditionalLinkArguments(String s) {
        baseScheme.setAdditionalLinkArguments(s);
    }

    public void clearAdditionalLinkArguments() {
        baseScheme.clearAdditionalLinkArguments();
    }

    public URL getURLForIndex(NamedObjectType type) {
        return append(baseScheme.getURLForIndex(type));
    }

    public URL getURLForRelativePage(String pageRelativeToBase) {
        return append(baseScheme.getURLForRelativePage(pageRelativeToBase));
    }

    public URL getURLForAbsolutePage(URL pageURL) {
        return append(baseScheme.getURLForAbsolutePage(pageURL));
    }

    @Override
    public URL getURLForApi(NamedObjectType type) {
        return baseScheme.getURLForApi(type); // cannot bookmark
    }
}
