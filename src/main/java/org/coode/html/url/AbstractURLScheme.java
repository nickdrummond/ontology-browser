/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.url;

import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.kit.OWLHTMLKit;
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
 * Date: Jan 12, 2008<br><br>
 */
public abstract class AbstractURLScheme implements URLScheme {

    private static final Logger logger = LoggerFactory.getLogger(AbstractURLScheme.class);

    protected final OWLHTMLKit kit;


    public AbstractURLScheme(OWLHTMLKit kit) {
        this.kit = kit;
    }

    public NamedObjectType getType(URL url) {
        try{
            return NamedObjectType.valueOf(getTypeString(url));
        }
        catch(IllegalArgumentException e){
            // do nothing
        }
        return null;
    }

    public String getTypeString(URL url){
        String relativeURL = URLUtils.createRelativeURL(kit.getBaseURL(), url);
        String[] path = relativeURL.split("/");
        return path[0]; // always the first element
    }

    public URL getURLForIndex(NamedObjectType type) {
        try {
            return new URL(kit.getBaseURL(), type.toString() + "/");
        }
        catch (MalformedURLException e) {
            logger.error("Could not create URL for index: " + type, e);
        }
        return null;
    }

    public URL getURLForRelativePage(String pageRelativeToBase) {
        try {
            return new URL(getBaseURL() + pageRelativeToBase);
        }
        catch (MalformedURLException e) {
            logger.error("Could not create URL for page: " + pageRelativeToBase, e);
        }
        return null;
    }


    public URL getURLForAbsolutePage(URL pageURL) {
        return pageURL;
    }

    public URL getBaseURL(){
        return kit.getBaseURL();
    }

    public OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }
}
