package org.ontbrowser.www.feature.stats;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

class ClassDescendantsStats implements Stats<OWLClass>{

    public static final String NAME = "classDescendants";

    // TODO cache

    private OWLReasoner reasoner;

    public ClassDescendantsStats(OWLReasoner reasoner) {
        this.reasoner = reasoner;
    }

    public int getStats(OWLClass cls) {
        return reasoner.getSubClasses(cls, false).getFlattened().size() - 1; // don't include Bottom
    }

    @Override
    public String getName() {
        return NAME;
    }
}
