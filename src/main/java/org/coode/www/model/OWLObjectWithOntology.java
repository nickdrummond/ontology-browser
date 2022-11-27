package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Objects;

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

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        OWLObjectWithOntology that = (OWLObjectWithOntology) o;
        return owlObject.equals(that.owlObject) && owlOntology.equals(that.owlOntology);
    }

    @Override
    public int hashCode() {
        return Objects.hash(owlObject, owlOntology);
    }
}
