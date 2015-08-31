package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

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
    protected Node<OWLClass> topNode() {
        return reasoner.getTopClassNode();
    }

    @Override
    protected Set<Node<OWLClass>> subs(OWLClass cls) {
        return reasoner.getSubClasses(cls, true).getNodes();
    }

    @Override
    protected Set<Node<OWLClass>> ancestors(OWLClass cls) {
        return reasoner.getSuperClasses(cls, false).getNodes();
    }

    @Override
    protected Node<OWLClass> equivs(OWLClass cls) {
        return reasoner.getEquivalentClasses(cls);
    }
}
