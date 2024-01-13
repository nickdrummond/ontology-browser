package org.coode.www.model.timeline;

public record TProp<T>(
        T type,
        String meta
) {
    public TProp(T type) {
        this(type, "");
    }

    public TProp<T> withMeta(String meta) {
        return new TProp<>(type, meta);
    }
}
