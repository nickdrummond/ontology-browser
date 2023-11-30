package org.coode.www.service.hierarchy;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.Node;

import java.util.Comparator;
import java.util.List;
import java.util.Set;

public abstract class AbstractOWLHierarchyService<T extends OWLObject> implements OWLHierarchyService<T> {

    private final Comparator<? super Tree<T>> comparator;

    public AbstractOWLHierarchyService(final Comparator<? super Tree<T>> comparator) {
        this.comparator = comparator;
    }

    @Override
    public Tree<T> getPrunedTree(final T entity) {
        Set<Node<T>> nodes = Sets.newHashSet(ancestors(entity));
        nodes.add(equivs(entity));
        return buildTree(topNode(), nodes);
    }

    public Tree<T> getTree() {
        return buildTree(topNode());
    }

    @Override
    public Tree<T> getChildren(T base) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(base)) {
            if (!subNode.isBottomNode()) {
                subs.add(new Tree<>(subNode));
            }
        }
        subs.sort(comparator);
        return new Tree<>(equivs(base), subs);
    }

    private Tree<T> buildTree(final Node<T> current, final Set<Node<T>> ancestors) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(current.getRepresentativeElement())) {
            //noinspection StatementWithEmptyBody
            if (subNode.isBottomNode()) {
                // ignore Nothing
            } else if (ancestors.contains(subNode)) { // recurse
                subs.add(buildTree(subNode, without(ancestors, subNode)));
            } else {
                subs.add(new Tree<>(subNode));
            }
        }
        subs.sort(comparator);
        return new Tree<>(current, subs);
    }

    private Tree<T> buildTree(final Node<T> current) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(current.getRepresentativeElement())) {
            //noinspection StatementWithEmptyBody
            if (subNode.isBottomNode()) {
                // ignore Nothing
            } else {
                subs.add(buildTree(subNode));
            }
        }
        subs.sort(comparator);
        return new Tree<>(current, subs);
    }

    private Set<Node<T>> without(final Set<Node<T>> original, final Node<T> node) {
        Set<Node<T>> nodes = Sets.newHashSet(original);
        nodes.remove(node);
        return nodes;
    }

    public boolean treeContains(T entity) {
        return true;
    }

    protected abstract Node<T> topNode();

    protected abstract Set<Node<T>> subs(T entity);

    protected abstract Set<Node<T>> ancestors(T entity);

    protected abstract Node<T> equivs(T entity);
}
