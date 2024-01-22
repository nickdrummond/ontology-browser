package org.coode.www.model.timeline;

import java.util.*;

public record Timeline (
        List<TConn> events,
        TProp endProp,
        boolean diverge,
        boolean converge
){}
