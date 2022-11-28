package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;

public abstract class AbstractURLScheme implements URLScheme {

    protected final OWLHTMLKit kit;

    public AbstractURLScheme(OWLHTMLKit kit) {
        this.kit = kit;
    }

    public OWLHTMLKit getOWLHTMLKit(){
        return kit;
    }
}
