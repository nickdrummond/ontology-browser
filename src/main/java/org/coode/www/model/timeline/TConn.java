package org.coode.www.model.timeline;

import java.util.List;

public record TConn(TProp prop, TNode node) {
    public TConn(TProp prop, String eName) {
        this(prop, new TEvent(eName));
    }
    public TConn(TProp prop, List<Timeline> timelines) {
        this(prop, new TParallel(timelines));
    }
}
