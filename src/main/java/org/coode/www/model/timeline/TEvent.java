package org.coode.www.model.timeline;

public record TEvent (
        String label,
        String meta
) implements TNode {
    public TEvent(String label) {
        this(label, "");
    }
}
