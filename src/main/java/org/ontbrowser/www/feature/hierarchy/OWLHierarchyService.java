package org.ontbrowser.www.feature.hierarchy;

import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.Node;

import java.util.stream.Stream;

public interface OWLHierarchyService<T extends OWLObject> {

    Tree<T> getTree();

    Tree<T> getPrunedTree(T focus);

    Tree<T> getChildren(T base);

    Stream<Node<T>> getDescendants(T t);
}
