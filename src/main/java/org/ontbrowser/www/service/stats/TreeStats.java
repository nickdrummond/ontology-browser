package org.ontbrowser.www.service.stats;

import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.service.hierarchy.OWLHierarchyService;
import org.semanticweb.owlapi.model.OWLObject;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class TreeStats<T extends OWLObject> implements Stats<T> {

    private final Map<T, Integer> cache = new HashMap<>();

    private final OWLHierarchyService<T> hierarchyService;

    public TreeStats(OWLHierarchyService<T> hierarchyService) {
        this.hierarchyService = hierarchyService;
    }

    @Override
    public int getStats(T target) {
        return getStats(target, new HashSet<>());
    }

    public int getStats(T target, Set<T> ancestors) {
        if (cache.containsKey(target)) {
            return cache.get(target);
        }
        Tree<T> tree = hierarchyService.getChildren(target);
        int result = tree.childCount;
        if (!ancestors.contains(target)) { // loop detection
            ancestors.add(target);
            result = result + tree.children.stream()
                    .map(child -> getStats(child.value.iterator().next(), ancestors)).mapToInt(i -> i).sum();
        }
        cache.put(target, result);
        return result;
    }

    @Override
    public String getName() {
        return "TreeStats";
    }
}
