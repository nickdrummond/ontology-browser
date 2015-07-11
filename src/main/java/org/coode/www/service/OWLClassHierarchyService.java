package org.coode.www.service;

import com.google.common.collect.Lists;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;
import org.springframework.util.StringUtils;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class OWLClassHierarchyService {

    private final OWLReasoner reasoner;
    private final Comparator<? super Tree<OWLClass>> comparator;

    public OWLClassHierarchyService(final OWLReasoner reasoner,
                                    final Comparator<? super Tree<OWLClass>> comparator) {
        this.reasoner = reasoner;
        this.comparator = comparator;
    }

    public Tree<OWLClass> getPrunedTree(final OWLClass aClass) {
        Node<OWLClass> top = reasoner.getTopClassNode();
        if (aClass.isTopEntity()) {
            return new Tree<>(top);
        }
        else {
            NodeSet<OWLClass> ancestors = reasoner.getSuperClasses(aClass, false);
            return buildTree(top, nodeSetWithout(ancestors, top));
        }
    }

    private Tree<OWLClass> buildTree(final Node<OWLClass> current, final NodeSet<OWLClass> ancestors) {
        List<Tree<OWLClass>> subs = Lists.newArrayList();
        for (Node<OWLClass> s : reasoner.getSubClasses(current.getRepresentativeElement(), true)) {
            if (s.isBottomNode()) {
                // ignore Nothing
            }
            else if (ancestors.containsEntity(s.getRepresentativeElement())) { // recurse
                subs.add(buildTree(s, nodeSetWithout(ancestors, s)));
            }
            else {
                subs.add(new Tree<>(s));
            }
        }
        subs.sort(comparator);
        return new Tree<>(current, subs);
    }

    private NodeSet<OWLClass> nodeSetWithout(final NodeSet<OWLClass> original, final Node<OWLClass> node) {
        Set<Node<OWLClass>> nodes = original.getNodes();
        nodes.remove(node);
        return new OWLClassNodeSet(nodes);
    }

    public static class Tree<T extends OWLEntity> {
        public final Node<T> value;
        public final List<Tree<T>> children;

        public Tree(final Node<T> value) {
            this(value, Collections.EMPTY_LIST);
        }

        public Tree(final Node<T> value, final List<Tree<T>> children) {
            this.value = value;
            this.children = children;
        }

        @Override
        public String toString() {
            String node = "\"node\": \"[" + StringUtils.collectionToCommaDelimitedString(value.getEntities()) + "]\"";
            String subs = "\"children\": [" + StringUtils.collectionToCommaDelimitedString(children) + "]";
            return "\n{" + node + "," + subs + "}";
        }
    }
}
