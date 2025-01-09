package org.ontbrowser.www.service.stats;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

class RelationsCountStats implements Stats<OWLObjectProperty> {

    public static final String NAME = "relationsCount";

    // TODO centralize cache
    private Map<OWLObjectPropertyExpression, Integer> cache = new HashMap<>();
    private Map<OWLObjectProperty, Set<OWLObjectProperty>> subsCache = new HashMap<>();

    private OWLReasoner reasoner;

    private OWLOntology ont;

    public RelationsCountStats(OWLReasoner reasoner) {
        this.reasoner = reasoner;
        this.ont = reasoner.getRootOntology();
    }

    public int getStats(OWLObjectProperty target) {
        if (cache.containsKey(target)) {
            return cache.get(target);
        }
        int directRelations = getNumberOfRelations(target);
        Set<OWLObjectProperty> subs = getNamedSubs(target);
        int result = directRelations + subs.stream().map(child -> getStats(child)).mapToInt(i -> i).sum();
        cache.put(target, result);
        return result;
    }

    private Set<OWLObjectProperty> getNamedSubs(OWLObjectProperty prop) {
        if (subsCache.containsKey(prop)) {
            return subsCache.get(prop);
        }

        Set<OWLObjectProperty> subs = reasoner.subObjectProperties(prop)
                .filter(s -> s.isNamed())
                .map(s -> s.asOWLObjectProperty())
                .collect(Collectors.toSet());
        subsCache.put(prop, subs);
        return subs;
    }

    @Override
    public String getName() {
        return NAME;
    }

    private int getNumberOfRelations(OWLObjectPropertyExpression prop) {
        if (cache.containsKey(prop)) {
            return cache.get(prop);
        }
        int size = ont.axioms(AxiomType.OBJECT_PROPERTY_ASSERTION, Imports.INCLUDED)
                .filter(ax -> ax.getProperty().equals(prop)).toList().size();
        cache.put(prop, size);
        return size;
    }
}
