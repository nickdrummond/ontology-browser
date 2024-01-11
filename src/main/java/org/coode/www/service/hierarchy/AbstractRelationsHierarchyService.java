package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public abstract class AbstractRelationsHierarchyService<T extends OWLProperty> extends AbstractHierarchyService<Relation<T>> {

    protected T property;

    protected List<T> properties;

    protected boolean inverse;

    protected OWLOntology ont;

    protected OWLNamedIndividual root;

    protected AbstractRelationsHierarchyService() {
        super();
    }

    protected AbstractRelationsHierarchyService(Comparator<? super Tree<Relation<T>>> comparator) {
        super(comparator);
    }

    public AbstractRelationsHierarchyService<T> withProperties(final T property,
                               final OWLOntology ont,
                               final boolean inverse) {
        this.property = property;
        this.properties = new ArrayList<>();
        this.properties.add(property);
        this.ont = ont;
        this.inverse = inverse;
        // dummy root - pun the property to avoid the generic tree
        this.root = ont.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(property.getIRI());

        return this;
    }

    public AbstractRelationsHierarchyService<T> withMoreProperties(final T... properties) {
        this.properties.addAll(Arrays.stream(properties).toList());
        return this;
    }

    public boolean isInverse() {
        return inverse;
    }

    public T getProperty() {
        return property;
    }

    public List<T> getProperties() {
        return properties;
    }
}
