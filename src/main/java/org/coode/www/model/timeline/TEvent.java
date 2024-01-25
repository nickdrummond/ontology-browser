package org.coode.www.model.timeline;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

public record TEvent (
        OWLNamedIndividual label,
        String meta
) implements TNode {
    public TEvent(OWLNamedIndividual label) {
        this(label, "");
    }

    @Override
    public String toString() {
        return label.getIRI().getIRIString();
    }
}
