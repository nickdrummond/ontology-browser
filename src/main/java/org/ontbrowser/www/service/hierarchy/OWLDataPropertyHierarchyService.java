package org.ontbrowser.www.service.hierarchy;

import org.ontbrowser.www.model.Tree;
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
    protected Node<OWLDataProperty> topNode() {
        return reasoner.getTopDataPropertyNode();
    }

    @Override
    protected Set<Node<OWLDataProperty>> subs(OWLDataProperty prop) {
        return reasoner.getSubDataProperties(prop, true).getNodes();
    }

    @Override
    protected Set<Node<OWLDataProperty>> ancestors(OWLDataProperty prop) {
        return reasoner.getSuperDataProperties(prop, false).getNodes();
    }

    @Override
    protected Node<OWLDataProperty> equivs(OWLDataProperty prop) {
        return reasoner.getEquivalentDataProperties(prop);
    }
}
