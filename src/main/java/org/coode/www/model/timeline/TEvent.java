package org.coode.www.model.timeline;

public record TEvent<T> (
        T label,
        String meta
) implements TNode {
    public TEvent(T label) {
        this(label, "");
    }
}
