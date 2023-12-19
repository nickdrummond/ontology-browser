package org.coode.www.model.timeline;

import java.util.*;

// TODO which way around to the props associate?
public record Timeline (
        TProp startProp,
        List<TConn> events,
        boolean diverge,
        boolean converge
){}
