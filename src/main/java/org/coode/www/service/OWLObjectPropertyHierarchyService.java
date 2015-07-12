package org.coode.www.service;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNodeSet;

import java.util.Comparator;
import java.util.Set;

public class OWLObjectPropertyHierarchyService extends AbstractOWLHierarchyService<OWLObjectPropertyExpression> {

    public OWLObjectPropertyHierarchyService(final OWLReasoner reasoner,
                                             final Comparator<? super Tree<OWLObjectPropertyExpression>> comparator) {
        super(reasoner, comparator);
    }

    @Override
    protected NodeSet<OWLObjectPropertyExpression> newNodeSet(Set<Node<OWLObjectPropertyExpression>> nodes) {
        return new OWLObjectPropertyNodeSet(nodes);
    }

    @Override
    protected Node<OWLObjectPropertyExpression> topNode() {
        return reasoner.getTopObjectPropertyNode();
    }

    @Override
    protected NodeSet<OWLObjectPropertyExpression> subs(OWLObjectPropertyExpression prop) {
        return reasoner.getSubObjectProperties(prop, true);
    }

    @Override
    protected NodeSet<OWLObjectPropertyExpression> ancestors(OWLObjectPropertyExpression prop) {
        return reasoner.getSuperObjectProperties(prop, false);
    }

    @Override
    protected Node<OWLObjectPropertyExpression> equivs(OWLObjectPropertyExpression prop) {
        return reasoner.getEquivalentObjectProperties(prop);
    }
}
