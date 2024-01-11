package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

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
    protected boolean isBottomNode(Set<OWLDataProperty> subNode) {
        return false;
    }

    @Override
    protected Set<OWLDataProperty> topNode() {
        return reasoner.getTopDataPropertyNode().getEntities();
    }

    @Override
    protected Set<Set<OWLDataProperty>> subs(OWLDataProperty prop) {
        return nodesToSet(reasoner.getSubDataProperties(prop, true).getNodes());
    }

    @Override
    protected Set<Set<OWLDataProperty>> ancestors(OWLDataProperty prop) {
        return nodesToSet(reasoner.getSuperDataProperties(prop, false).getNodes());
    }

    @Override
    protected Set<OWLDataProperty> equivs(OWLDataProperty prop) {
        return reasoner.getEquivalentDataProperties(prop).getEntities();
    }
}
