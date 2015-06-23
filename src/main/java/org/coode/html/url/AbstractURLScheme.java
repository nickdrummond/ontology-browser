package org.coode.html.url;

import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.kit.OWLHTMLKit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.MalformedURLException;
import java.net.URL;

public abstract class AbstractURLScheme implements URLScheme {

    private static final Logger logger = LoggerFactory.getLogger(AbstractURLScheme.class);

    protected final OWLHTMLKit kit;

    public AbstractURLScheme(OWLHTMLKit kit) {
        this.kit = kit;
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

    public URL getBaseURL(){
        return kit.getBaseURL();
    }

    public OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }
}
