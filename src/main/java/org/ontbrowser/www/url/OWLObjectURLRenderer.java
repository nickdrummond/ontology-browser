package org.ontbrowser.www.url;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

public interface OWLObjectURLRenderer {
    String getURLForOWLObject(OWLObject owlObject, OWLOntology ontology);
}
