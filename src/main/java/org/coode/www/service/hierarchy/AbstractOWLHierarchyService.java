package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.Node;

import java.util.Comparator;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class AbstractOWLHierarchyService<T extends OWLObject> extends AbstractHierarchyService<T> {

    protected AbstractOWLHierarchyService(Comparator<? super Tree<T>> comparator) {
        super(comparator);
    }

    @Override
    protected T getRepresentativeElement(Set<T> node) {
        return node.iterator().next();
    }

    protected Set<Set<T>> nodesToSet(Set<Node<T>> nodes) {
        return nodes.stream().map(Node::getEntities).collect(Collectors.toSet());
    }

}
