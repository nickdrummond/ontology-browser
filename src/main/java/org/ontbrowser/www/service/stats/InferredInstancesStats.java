package org.ontbrowser.www.service.stats;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

class InferredInstancesStats implements Stats<OWLClass>{
    public static final String NAME = "inferredInstances";

    // TODO cache

    private OWLReasoner reasoner;

    public InferredInstancesStats(OWLReasoner reasoner) {
        this.reasoner = reasoner;
    }

    public int getStats(OWLClass cls) {
        return reasoner.getInstances(cls, false).getFlattened().size();
    }

    @Override
    public String getName() {
        return NAME;
    }
}
