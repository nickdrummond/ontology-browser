package org.coode.www.model.timeline;

import java.util.List;

public record TParallel<T, P>(
        List<Timeline<T, P>> timelines
) implements TNode { }
