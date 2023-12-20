package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;

public interface OWLHierarchyService<T extends OWLObject> {

    Tree<T> getPrunedTree(T focus);

    Tree<T> getChildren(T base);

    Tree<T> getSubtree(T root);
}
