package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLObject;
import org.thymeleaf.util.StringUtils;

import java.util.Collections;
import java.util.List;

public class Tree<T extends OWLObject> {
    public final Iterable<T> value;
    public final List<Tree<T>> children;

    public Tree(final Iterable<T> value) {
        this(value, Collections.emptyList());
    }

    public Tree(final Iterable<T> value, final List<Tree<T>> children) {
        this.value = value;
        this.children = children;
    }

    @Override
    public String toString() {
        String node = "\"node\": \"[" + StringUtils.join(value, ", ") + "]\"";
        String subs = "\"children\": [" + StringUtils.join(children, ", ") + "]";
        return "\n{" + node + "," + subs + "}";
    }
}
