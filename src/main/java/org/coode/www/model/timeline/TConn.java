package org.coode.www.model.timeline;

import java.util.List;

public record TConn<T, P>(TProp prop, TNode node) {
    public TConn(TProp prop, T eName) {
        this(prop, new TEvent<>(eName));
    }
    public TConn(TProp prop, List<Timeline<T, P>> timelines) {
        this(prop, new TParallel<>(timelines));
    }
}
