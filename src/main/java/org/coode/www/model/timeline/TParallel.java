package org.coode.www.model.timeline;

import java.util.List;

public record TParallel(
        List<Timeline> timelines
) implements TNode { }
