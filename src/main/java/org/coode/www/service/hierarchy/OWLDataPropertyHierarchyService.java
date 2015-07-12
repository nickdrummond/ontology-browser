package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLDataPropertyNodeSet;

import java.util.Comparator;
import java.util.Set;

public class OWLDataPropertyHierarchyService extends AbstractOWLHierarchyService<OWLDataProperty> {

    private final OWLReasoner reasoner;

    public OWLDataPropertyHierarchyService(final OWLReasoner reasoner,
                                           final Comparator<? super Tree<OWLDataProperty>> comparator) {
        super(comparator);
        this.reasoner = reasoner;
    }

    @Override
    protected NodeSet<OWLDataProperty> newNodeSet(Set<Node<OWLDataProperty>> nodes) {
        return new OWLDataPropertyNodeSet(nodes);
    }

    @Override
    protected Node<OWLDataProperty> topNode() {
        return reasoner.getTopDataPropertyNode();
    }

    @Override
    protected NodeSet<OWLDataProperty> subs(OWLDataProperty prop) {
        return reasoner.getSubDataProperties(prop, true);
    }

    @Override
    protected NodeSet<OWLDataProperty> ancestors(OWLDataProperty prop) {
        return reasoner.getSuperDataProperties(prop, false);
    }

    @Override
    protected Node<OWLDataProperty> equivs(OWLDataProperty prop) {
        return reasoner.getEquivalentDataProperties(prop);
    }
}
