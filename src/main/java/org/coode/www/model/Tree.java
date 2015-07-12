package org.coode.www.model;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.reasoner.Node;
import org.springframework.util.StringUtils;

import java.util.Collections;
import java.util.List;

public class Tree<T extends OWLObject> {
    public final Node<T> value;
    public final List<Tree<T>> children;

    public Tree(final Node<T> value) {
        this(value, Collections.emptyList());
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
