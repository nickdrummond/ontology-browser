package org.coode.www.model.timeline;

public record TParent (
        String label,
        Timeline children
) implements TNode { }
