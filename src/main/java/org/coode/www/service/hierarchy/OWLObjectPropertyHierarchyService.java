package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.IsAnonymous;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNode;

import java.util.Comparator;
import java.util.Set;
import java.util.stream.Collectors;

public class OWLObjectPropertyHierarchyService extends AbstractOWLHierarchyService<OWLObjectPropertyExpression> {

    private final OWLReasoner reasoner;

    public OWLObjectPropertyHierarchyService(final OWLReasoner reasoner,
                                             final Comparator<? super Tree<OWLObjectPropertyExpression>> comparator) {
        super(comparator);
        this.reasoner = reasoner;
    }

    @Override
    protected Node<OWLObjectPropertyExpression> topNode() {
        return reasoner.getTopObjectPropertyNode();
    }

    @Override
    protected Set<Node<OWLObjectPropertyExpression>> subs(OWLObjectPropertyExpression prop) {
        return reasoner.getSubObjectProperties(prop, true).nodes()
                .filter(node -> node.entities().anyMatch(IsAnonymous::isNamed))
                .map(this::stripNode)
                .collect(Collectors.toSet());
    }

    @Override
    protected Set<Node<OWLObjectPropertyExpression>> ancestors(OWLObjectPropertyExpression prop) {
        return reasoner.getSuperObjectProperties(prop, false).nodes()
                .map(this::stripNode)
                .collect(Collectors.toSet());
    }

    @Override
    protected Node<OWLObjectPropertyExpression> equivs(OWLObjectPropertyExpression prop) {
        return stripNode(reasoner.getEquivalentObjectProperties(prop));
    }

    // Remove inverseOf properties from equivalents
    private OWLObjectPropertyNode stripNode(Node<OWLObjectPropertyExpression> n) {
        return new OWLObjectPropertyNode(n.entities().filter(IsAnonymous::isNamed));
    }
}
