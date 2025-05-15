package org.ontbrowser.www.feature.stats;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

class TransitiveRelationsStats implements Stats<OWLNamedIndividual>{
    public static final String NAME = "transitiveRelationsCount";

    // TODO cache

    private OWLReasoner reasoner;
    private final OWLObjectProperty prop;

    public TransitiveRelationsStats(OWLReasoner reasoner, OWLObjectProperty prop) {
        this.reasoner = reasoner;
        this.prop = prop;
    }

    public int getStats(OWLNamedIndividual ind) {
        return reasoner.getObjectPropertyValues(ind, prop).getFlattened().size();
    }

    @Override
    public String getName() {
        return NAME;
    }
}
