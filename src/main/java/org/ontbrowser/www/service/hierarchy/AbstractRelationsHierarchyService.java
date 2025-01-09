package org.ontbrowser.www.service.hierarchy;

import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;

public abstract class AbstractRelationsHierarchyService<T extends OWLProperty>
        extends AbstractOWLHierarchyService<OWLNamedIndividual> {

    protected T property;

    protected boolean inverse;

    protected OWLOntology ont;

    protected OWLNamedIndividual root;

    protected AbstractRelationsHierarchyService(Comparator<? super Tree<OWLNamedIndividual>> comparator) {
        super(comparator);
    }

    public AbstractRelationsHierarchyService<T> withProperties(final T property,
                               final OWLOntology ont,
                               final boolean inverse) {
        this.property = property;
        this.ont = ont;
        this.inverse = inverse;
        // dummy root - pun the property to avoid the generic tree
        this.root = ont.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(property.getIRI());

        return this;
    }

    public OWLOntology getOnt() {
        return ont;
    }

    public boolean isInverse() {
        return inverse;
    }

    public T getProperty() {
        return property;
    }
}
