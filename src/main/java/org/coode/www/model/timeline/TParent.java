package org.coode.www.model.timeline;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

public record TParent (
        OWLNamedIndividual label,
        Timeline children
) implements TNode { }
