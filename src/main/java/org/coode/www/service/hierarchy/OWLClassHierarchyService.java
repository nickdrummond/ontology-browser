package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLClass;
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
    protected boolean isBottomNode(Set<OWLClass> subNode) {
        return reasoner.getBottomClassNode().getEntities().equals(subNode);
    }

    @Override
    protected Set<OWLClass> topNode() {
        return reasoner.getTopClassNode().getEntities();
    }

    @Override
    protected Set<Set<OWLClass>> subs(OWLClass cls) {
        return nodesToSet(reasoner.getSubClasses(cls, true).getNodes());
    }

    @Override
    protected Set<Set<OWLClass>> ancestors(OWLClass cls) {
        return nodesToSet(reasoner.getSuperClasses(cls, false).getNodes());
    }

    @Override
    protected Set<OWLClass> equivs(OWLClass cls) {
        return reasoner.getEquivalentClasses(cls).getEntities();
    }

}
