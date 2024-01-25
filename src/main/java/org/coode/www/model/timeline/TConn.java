package org.coode.www.model.timeline;

import org.semanticweb.owlapi.model.OWLNamedIndividual;

import java.util.List;

public record TConn(TProp prop, TNode node) {
    public TConn(TProp prop, OWLNamedIndividual eName) {
        this(prop, new TEvent(eName));
    }
    public TConn(TProp prop, List<Timeline> timelines) {
        this(prop, new TParallel(timelines));
    }

    @Override
    public String toString() {
        return "(" +
                prop +
                "->" + node +
                ')';
    }
}
