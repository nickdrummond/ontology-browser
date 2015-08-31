package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;

public abstract class AbstractURLScheme implements URLScheme {

    private static final Logger logger = LoggerFactory.getLogger(AbstractURLScheme.class);

    protected final OWLHTMLKit kit;

    public AbstractURLScheme(OWLHTMLKit kit) {
        this.kit = kit;
    }

    public URL getBaseURL(){
        return kit.getBaseUrl();
    }

    public OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }
}
