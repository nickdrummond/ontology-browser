package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;

import java.util.Comparator;
import java.util.Set;

public class OWLClassHierarchyService extends AbstractOWLHierarchyService<OWLClass> {

    private final OWLReasoner reasoner;

    public OWLClassHierarchyService(final OWLReasoner reasoner,
                                    final Comparator<? super Tree<OWLClass>> comparator) {
        super(comparator);
        this.reasoner = reasoner;
    }

    @Override
    protected NodeSet<OWLClass> newNodeSet(Set<Node<OWLClass>> nodes) {
        return new OWLClassNodeSet(nodes);
    }

    @Override
    protected Node<OWLClass> topNode() {
        return reasoner.getTopClassNode();
    }

    @Override
    protected NodeSet<OWLClass> subs(OWLClass cls) {
        return reasoner.getSubClasses(cls, true);
    }

    @Override
    protected NodeSet<OWLClass> ancestors(OWLClass cls) {
        return reasoner.getSuperClasses(cls, false);
    }

    @Override
    protected Node<OWLClass> equivs(OWLClass cls) {
        return reasoner.getEquivalentClasses(cls);
    }
}
