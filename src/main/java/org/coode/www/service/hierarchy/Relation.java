package org.coode.www.service.hierarchy;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLProperty;

import javax.annotation.Nonnull;
import java.util.Objects;

public record Relation<P extends OWLProperty>(@Nonnull P property, @Nonnull OWLNamedIndividual individual) {
    public Relation {
        Objects.requireNonNull(individual);
        Objects.requireNonNull(property);
    }
}
