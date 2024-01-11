package org.coode.www.model;

import com.google.common.collect.Lists;
import org.thymeleaf.util.StringUtils;

import java.util.Collections;
import java.util.List;

public class Tree<T> {
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
        return "Tree:\n" + indented(this, 0);
    }

    private String indented(Tree<T> t, int indent) {
        StringBuilder sb = new StringBuilder();
        sb.append(StringUtils.repeat("\t", indent));
        sb.append("[");
        sb.append(StringUtils.join(t.value, ", "));
        sb.append("]");
        sb.append("(" + t.childCount + ")");
        sb.append("\n");
        for (Tree<T> child: t.children ) {
            sb.append(indented(child, indent+1));
        }
        return sb.toString();
    }
}
