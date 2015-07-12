package org.coode.www.service;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;

import java.util.Comparator;
import java.util.List;
import java.util.Set;

public abstract class AbstractOWLHierarchyService<T extends OWLEntity> {

    protected final OWLReasoner reasoner;
    private final Comparator<? super Tree<T>> comparator;

    public AbstractOWLHierarchyService(final OWLReasoner reasoner,
                                       final Comparator<? super Tree<T>> comparator) {
        this.reasoner = reasoner;
        this.comparator = comparator;
    }

    public Tree<T> getPrunedTree(final T entity) {
        NodeSet<T> ancestors = ancestors(entity);
        Set<Node<T>> nodes = Sets.newHashSet(ancestors.getNodes());
        nodes.add(equivs(entity));
        return buildTree(topNode(), newNodeSet(nodes));
    }

    private Tree<T> buildTree(final Node<T> current, final NodeSet<T> ancestors) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(current.getRepresentativeElement())) {
            if (subNode.isBottomNode()) {
                // ignore Nothing
            }
            else if (ancestors.getNodes().contains(subNode)) { // recurse
                subs.add(buildTree(subNode, nodeSetWithout(ancestors, subNode)));
            }
            else {
                subs.add(new Tree<>(subNode));
            }
        }
        subs.sort(comparator);
        return new Tree<>(current, subs);
    }

    private NodeSet<T> nodeSetWithout(final NodeSet<T> original, final Node<T> node) {
        Set<Node<T>> nodes = original.getNodes();
        nodes.remove(node);
        return newNodeSet(nodes);
    }

    protected abstract NodeSet<T> newNodeSet(Set<Node<T>> nodes);
    protected abstract Node<T> topNode();
    protected abstract NodeSet<T> subs(T entity);
    protected abstract NodeSet<T> ancestors(T entity);
    protected abstract Node<T> equivs(T entity);
}
