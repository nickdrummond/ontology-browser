package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

public class OWLObjectWithOntology {
    private final OWLObject owlObject;
    private final OWLOntology owlOntology;

    public OWLObjectWithOntology(OWLObject owlObject, OWLOntology owlOntology) {
        this.owlObject = owlObject;
        this.owlOntology = owlOntology;
    }

    public OWLObject getOWLObject() {
        return owlObject;
    }

    public OWLOntology getOWLOntology() {
        return owlOntology;
    }
}
