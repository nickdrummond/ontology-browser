package org.coode.www.util;

import org.semanticweb.owlapi.model.OWLEntity;

import java.util.HashSet;
import java.util.Set;

public class Utils {

    public static Set<OWLEntity> subtract(Set<OWLEntity> unwanted, Set<OWLEntity> from) {
        Set<OWLEntity> r = new HashSet<>(from);
        r.removeAll(unwanted);
        return r;
    }

}
