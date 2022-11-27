package org.coode.html.url;

import org.semanticweb.owlapi.model.OWLObject;

import java.net.URL;

public interface OWLObjectURLRenderer {
    String getURLForOWLObject(OWLObject owlObject);
}
