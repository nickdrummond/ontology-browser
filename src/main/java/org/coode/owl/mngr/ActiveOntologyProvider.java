package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.OWLOntology;

public interface ActiveOntologyProvider {

    OWLOntology getActiveOntology();
}
