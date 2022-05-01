package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;

public abstract class AbstractURLScheme implements URLScheme {

    protected final OWLHTMLKit kit;

    public AbstractURLScheme(OWLHTMLKit kit) {
        this.kit = kit;
    }

    public OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }
}
