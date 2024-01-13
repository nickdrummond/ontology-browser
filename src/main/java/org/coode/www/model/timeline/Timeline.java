package org.coode.www.model.timeline;

import java.util.*;

// TODO which way around to the props associate?
public record Timeline<T, P> (
        List<TConn<T, P>> events,
        TProp<P> endProp,
        boolean diverge,
        boolean converge
){}
