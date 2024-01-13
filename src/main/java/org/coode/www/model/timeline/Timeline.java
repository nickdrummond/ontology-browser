package org.coode.www.model.timeline;

import java.util.*;

// TODO which way around to the props associate?
public record Timeline (
        List<TConn> events,
        TProp endProp,
        boolean diverge,
        boolean converge
){}
