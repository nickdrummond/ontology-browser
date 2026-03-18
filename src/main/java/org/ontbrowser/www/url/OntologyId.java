package org.ontbrowser.www.url;

import org.semanticweb.owlapi.model.OWLOntologyID;

public class OntologyId {

    public static String getIdForOntology(OWLOntologyID ontId) {
        return String.valueOf(ontId.hashCode());
    }
}
