package org.ontbrowser.www.model;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Objects;

public class AxiomWithMetadata {
    private final OWLObject owlObject;

    private final OWLAxiom owlAxiom;
    private final OWLOntology owlOntology;
    private final String type;

    public AxiomWithMetadata(String type, OWLObject owlObject, OWLAxiom owlAxiom, OWLOntology owlOntology) {
        this.owlObject = owlObject;
        this.owlAxiom = owlAxiom;
        this.owlOntology = owlOntology;
        this.type = type;
    }

    public OWLObject getOWLObject() {
        return owlObject;
    }

    public OWLAxiom getOWLAxiom() {
        return owlAxiom;
    }

    public OWLOntology getOWLOntology() {
        return owlOntology;
    }

    public String getType() {
        return type;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AxiomWithMetadata that = (AxiomWithMetadata) o;
        return owlAxiom.equals(that.owlAxiom) && owlOntology.equals(that.owlOntology);
    }

    @Override
    public int hashCode() {
        return Objects.hash(owlAxiom, owlOntology);
    }
}
