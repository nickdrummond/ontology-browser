package org.ontbrowser.www.url;

import org.semanticweb.owlapi.model.OWLObject;

public interface OWLObjectURLRenderer {
    String getURLForOWLObject(OWLObject owlObject);
}
