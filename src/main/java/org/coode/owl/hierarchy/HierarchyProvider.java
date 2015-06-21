package org.coode.owl.hierarchy;

import java.util.Set;

public interface HierarchyProvider<O> {

    Class<? extends O> getNodeClass();

    Set<O> getRoots();

    boolean isRoot(O node);

    boolean isLeaf(O node);

    Set<O> getParents(O node);

    Set<O> getChildren(O node);

    Set<O> getEquivalents(O node);

    Set<O> getDescendants(O node);

    Set<O> getAncestors(O node);

    boolean hasAncestor(O node, O ancestor);

    void dispose();
}
