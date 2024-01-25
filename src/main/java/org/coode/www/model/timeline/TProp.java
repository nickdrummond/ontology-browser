package org.coode.www.model.timeline;

public record TProp(
        String type,
        String meta
) {
    public TProp(String type) {
        this(type, "");
    }

    public TProp withMeta(String meta) {
        return new TProp(type, meta);
    }

    @Override
    public String toString() {
        return type;
    }
}
