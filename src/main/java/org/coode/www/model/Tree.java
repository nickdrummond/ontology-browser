package org.coode.www.model;

import com.google.common.collect.Lists;
import org.semanticweb.owlapi.model.OWLObject;
import org.thymeleaf.util.StringUtils;

import java.util.Collections;
import java.util.List;

public class Tree<T extends OWLObject> {
    public final List<T> value;
    public final int childCount;
    public final List<Tree<T>> children;

    public Tree(final T value, int childCount) {
        this.value = Lists.newArrayList(value);
        this.childCount = childCount;
        this.children = Collections.emptyList();
    }

    public Tree(final Iterable<? extends T> value, int childCount) {
        this.value = Lists.newArrayList(value);
        this.childCount = childCount;
        this.children = Collections.emptyList();
    }

    public Tree(final T value, final List<Tree<T>> children) {
        this.value = Lists.newArrayList(value);
        this.childCount = children.size();
        this.children = children;
    }

    public Tree(final Iterable<? extends T> value, final List<Tree<T>> children) {
        this.value = Lists.newArrayList(value);
        this.childCount = children.size();
        this.children = children;
    }

    @Override
    public String toString() {
        String node = "node: [" + StringUtils.join(value, ", ") + "]";
        String subs = "\nchildren: [" + StringUtils.join(children, ",\n\t\t") + "]";
        return "{" + node + "(" + childCount + ")," + subs + "}";
    }
}
