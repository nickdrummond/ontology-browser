package org.coode.www.service.hierarchy;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class AbstractOWLHierarchyService<T extends OWLObject> implements OWLHierarchyService<T> {
    private final Logger logger = LoggerFactory.getLogger(getClass());

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
        return buildTreeFrom(topNode(), new HashSet<>());
    }

    @Override
    public Tree<T> getChildren(T base) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(base)) {
            if (!subNode.isBottomNode()) {
                subs.add(new Tree<>(subNode, getChildCount(subNode)));
            }
        }
        subs.sort(comparator);
        return new Tree<>(equivs(base), subs);
    }

    @Override
    public Tree<T> getSubtree(T root) {
        return buildTreeFrom(root, new HashSet<>());
    }

    // TODO cache
    private Tree<T> buildTree(final Node<T> current, final Set<Node<T>> ancestors) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(current.getRepresentativeElement())) {
            //noinspection StatementWithEmptyBody
            if (subNode.isBottomNode()) {
                // ignore Nothing
            } else if (ancestors.contains(subNode)) { // recurse
                subs.add(buildTree(subNode, without(ancestors, subNode)));
            } else {
                subs.add(new Tree<>(subNode, getChildCount(subNode))); // just the size
            }
        }
        subs.sort(comparator);
        return new Tree<>(current, subs);
    }

    private int getChildCount(Node<T> subNode) {
        Set<Node<T>> subs = subs(subNode.getRepresentativeElement());
        if (subs.size() == 1 && subs.iterator().next().isBottomNode()) {
            return 0;
        }
        return subs.size();
    }

    private Tree<T> buildTreeFrom(final Node<T> current, final Set<Node<T>> alreadyVisited) {
        return buildTreeFrom(current.getRepresentativeElement(), alreadyVisited);
    }

    private Tree<T> buildTreeFrom(final T current, final Set<Node<T>> alreadyVisited) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(current)) {
            //noinspection StatementWithEmptyBody
            if (subNode.isBottomNode()) {
                // ignore Nothing
            } else if (alreadyVisited.contains(subNode)) {
                logger.info("Found loop: " + subNode.getRepresentativeElement());
                subs.add(new Tree<>(subNode, getChildCount(subNode))); // just the size
            } else {
                alreadyVisited.add(subNode);
                subs.add(buildTree(subNode, alreadyVisited));
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
