package org.coode.www.model.timeline;

import java.util.List;

public record TConn(TNode node, TProp prop) {
    public TConn(String eName, TProp prop) {
        this(new TEvent(eName), prop);
    }
    public TConn(List<Timeline> timelines, TProp prop) {
        this(new TParallel(timelines), prop);
    }
}
