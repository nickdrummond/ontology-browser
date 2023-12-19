package org.coode.www.model.timeline;

import java.util.List;

public record TParent (
        String label,
        Timeline children
) implements TNode { }
