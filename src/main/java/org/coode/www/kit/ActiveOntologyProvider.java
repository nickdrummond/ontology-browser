package org.coode.www.kit;

import org.semanticweb.owlapi.model.OWLOntology;

public interface ActiveOntologyProvider {

    OWLOntology getActiveOntology();
}
