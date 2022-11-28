package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.OWLOntology;

public interface ActiveOntologyProvider {

    OWLOntology getActiveOntology();

    void addActiveOntologyListener(Listener l);

    void removeActiveOntologyListener(Listener l);

    interface Listener{
        void activeOntologyChanged(OWLOntology ont);
    }
}
