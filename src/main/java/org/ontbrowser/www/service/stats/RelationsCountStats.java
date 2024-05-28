package org.ontbrowser.www.service.stats;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.HashMap;
import java.util.Map;

class RelationsCountStats implements Stats<OWLObjectProperty> {

    public static final String NAME = "relationsCount";

    // TODO centralize cache
    private Map<OWLObjectPropertyExpression, Integer> cache = new HashMap<>();

    private OWLReasoner reasoner;

    private OWLOntology ont;

    public RelationsCountStats(OWLReasoner reasoner) {
        this.reasoner = reasoner;
        this.ont = reasoner.getRootOntology();
    }

    public int getStats(OWLObjectProperty prop) {
        int directRelations = getNumberOfRelations(prop);
        return directRelations + reasoner.subObjectProperties(prop, false).map(this::getNumberOfRelations).mapToInt(i -> i).sum();
    }

    @Override
    public String getName() {
        return NAME;
    }

    public int getStats(OWLNamedIndividual ind) {
        return 0;
    }

    private int getNumberOfRelations(OWLObjectPropertyExpression prop) {
        if (cache.containsKey(prop)) {
            return cache.get(prop);
        }
        int size = ont.axioms(AxiomType.OBJECT_PROPERTY_ASSERTION, Imports.INCLUDED).filter(ax -> ax.getProperty().equals(prop)).toList().size();
        cache.put(prop, size);
        return size;
    }
}
