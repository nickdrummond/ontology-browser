package org.coode.www.model.timeline;

public record TParent<T, P> (
        T label,
        Timeline<T, P> children
) implements TNode { }
