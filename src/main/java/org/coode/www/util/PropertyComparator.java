package org.coode.www.util;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.lang.NonNull;

import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

public class PropertyComparator implements Comparator<OWLObject> {

    private final OWLDataProperty property;
    private final Comparator<OWLObject> fallback;
    private final OWLReasoner r;

    public PropertyComparator(OWLDataProperty property, Comparator<OWLObject> fallback, OWLReasoner r) {
        this.property = property;
        this.fallback = fallback;
        this.r = r;
    }

    @Override
    public int compare(OWLObject o1, OWLObject o2) {
        return o1.accept(propertyExtr).orElse(Integer.MAX_VALUE).compareTo(
                o2.accept(propertyExtr).orElse(Integer.MAX_VALUE));
    }

    @Override
    public Comparator<OWLObject> thenComparing(Comparator<? super OWLObject> other) {
        return fallback;
    }

    private final OWLObjectVisitorEx<Optional<Integer>> propertyExtr = new OWLObjectVisitorEx<>() {

        @Override
        public Optional<Integer> visit(@NonNull OWLNamedIndividual individual) {
            Set<OWLLiteral> years = r.getDataPropertyValues(individual, property);
            return years.stream().filter(OWLLiteral::isInteger).map(OWLLiteral::parseInteger).findFirst();
        }

        @Override
        public Optional<Integer> doDefault(@NonNull Object object) {
            return Optional.empty();
        }
    };
}
