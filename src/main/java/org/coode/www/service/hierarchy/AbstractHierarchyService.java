package org.coode.www.service.hierarchy;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public abstract class AbstractHierarchyService<T> implements HierarchyService<T> {
    private final Logger logger = LoggerFactory.getLogger(getClass().getName());

    private final Comparator<? super Tree<T>> comparator;

    public AbstractHierarchyService() {
        this.comparator = null;
    }

    public AbstractHierarchyService(final Comparator<? super Tree<T>> comparator) {
        this.comparator = comparator;
    }

    @Override
    public Tree<T> getPrunedTree(final T entity) {
        Set<Set<T>> nodes = Sets.newHashSet(ancestors(entity));
        nodes.add(equivs(entity));
        return buildTree(topNode(), nodes);
    }

    public Tree<T> getTree() {
        return buildTreeFrom(topNode(), new HashSet<>());
    }

    @Override
    public Tree<T> getChildren(T base) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Set<T> subNode : subs(base)) {
            if (!isBottomNode(subNode)) {
                subs.add(new Tree<>(subNode, getChildCount(subNode)));
            }
        }
        if(comparator != null) {
            subs.sort(comparator);
        }
        return new Tree<>(equivs(base), subs);
    }

    @Override
    public Tree<T> getSubtree(T root) {
        return buildTreeFrom(root, new HashSet<>());
    }

    // TODO cache
    private Tree<T> buildTree(final Set<T> current, final Set<Set<T>> ancestors) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Set<T> subNode : subs(getRepresentativeElement(current))) {
            //noinspection StatementWithEmptyBody
            if (isBottomNode(subNode)) {
                // ignore Nothing
            } else if (ancestors.contains(subNode)) { // recurse
                subs.add(buildTree(subNode, without(ancestors, subNode)));
            } else {
                subs.add(new Tree<>(subNode, getChildCount(subNode))); // just the size
            }
        }
        if(comparator != null) {
            subs.sort(comparator);
        }
        return new Tree<>(current, subs);
    }

    private int getChildCount(Set<T> subNode) {
        Set<Set<T>> subs = subs(getRepresentativeElement(subNode));
        if (subs.size() == 1 && isBottomNode(subs.iterator().next())) {
            return 0;
        }
        return subs.size();
    }

    private Tree<T> buildTreeFrom(final Set<T> current, final Set<Set<T>> alreadyVisited) {
        return buildTreeFrom(getRepresentativeElement(current), alreadyVisited);
    }

    private Tree<T> buildTreeFrom(final T current, final Set<Set<T>> alreadyVisited) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Set<T> subNode : subs(current)) {
            //noinspection StatementWithEmptyBody
            if (isBottomNode(subNode)) {
                // ignore Nothing
            } else if (alreadyVisited.contains(subNode)) {
                logger.info("Found loop: {}", getRepresentativeElement(subNode));
                subs.add(new Tree<>(subNode, getChildCount(subNode))); // just the size
            } else {
                alreadyVisited.add(subNode);
                subs.add(buildTreeFrom(getRepresentativeElement(subNode), alreadyVisited));
            }
        }
        if(comparator != null) {
            subs.sort(comparator);
        }
        return new Tree<>(current, subs);
    }

    private Set<Set<T>> without(final Set<Set<T>> original, final Set<T> node) {
        Set<Set<T>> nodes = Sets.newHashSet(original);
        nodes.remove(node);
        return nodes;
    }

    public boolean treeContains(T entity) {
        return true;
    }

    protected abstract boolean isBottomNode(Set<T> subNode);

    protected abstract T getRepresentativeElement(Set<T> node);

    protected abstract Set<T> topNode();

    protected abstract Set<Set<T>> subs(T entity);

    protected abstract Set<Set<T>> ancestors(T entity);

    protected abstract Set<T> equivs(T entity);
}
