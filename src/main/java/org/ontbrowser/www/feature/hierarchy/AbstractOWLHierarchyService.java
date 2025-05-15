package org.ontbrowser.www.feature.hierarchy;

import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.google.common.collect.Streams;
import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.Node;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

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
        return buildAllTree(topNode(), new HashSet<>());
    }

    public Tree<T> getClosedTree() {
        Set<Node<T>> subNodes = subs(topNode().getRepresentativeElement());
        List<Tree<T>> subs = subNodes.stream().map(n -> new Tree<>(n, getChildCount(n))).sorted(comparator).toList();
        return new Tree<>(topNode(), subs, subNodes.size());
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
        Node<T> baseNode = equivs(base);
        return new Tree<>(baseNode, subs, getChildCount(baseNode));
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
        return new Tree<>(current, subs, getChildCount(current));
    }

    protected int getChildCount(Node<T> subNode) {
        Set<Node<T>> subs = subs(subNode.getRepresentativeElement());
        if (subs.size() == 1 && subs.iterator().next().isBottomNode()) {
            return 0;
        }
        return subs.size();
    }

    private Tree<T> buildAllTree(final Node<T> current, final Set<Node<T>> alreadyVisited) {
        List<Tree<T>> subs = Lists.newArrayList();
        for (Node<T> subNode : subs(current.getRepresentativeElement())) {
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
        return new Tree<>(current, subs, getChildCount(current));
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

    @Override
    public Stream<Node<T>> getDescendants(T t) {
        Set<Node<T>> subs = subs(t);
        Stream<Node<T>> recursive = subs.stream().flatMap(sub -> getDescendants(sub.getRepresentativeElement()));
        return Streams.concat(
                subs.stream(),
                recursive);
    }
}
