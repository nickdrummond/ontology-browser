package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;

public interface HierarchyService<T> {

    Tree<T> getPrunedTree(T focus);

    Tree<T> getChildren(T base);

    Tree<T> getSubtree(T root);
}
