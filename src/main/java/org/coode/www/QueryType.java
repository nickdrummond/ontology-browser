/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 6, 2008<br><br>
 */
public enum QueryType {
    subclasses,
    superclasses,
    equivalents,
    descendants,
    ancestors,
    instances;

    public Set<OWLEntity> getResults(OWLClassExpression descr, OWLHTMLKit kit) {
        Set<OWLEntity> results = new HashSet<OWLEntity>();
        final OWLReasoner r = kit.getOWLServer().getOWLReasoner();
        switch(this){
            case equivalents: results.addAll(r.getEquivalentClasses(descr).getEntities()); break;
            case subclasses: results.addAll(r.getSubClasses(descr, true).getFlattened()); break;
            case descendants: results.addAll(r.getSubClasses(descr, false).getFlattened()); break;
            case superclasses: results.addAll(r.getSuperClasses(descr, true).getFlattened()); break;
            case ancestors: results.addAll(r.getSuperClasses(descr, false).getFlattened()); break;
            case instances: results.addAll(r.getInstances(descr, false).getFlattened()); break;
        }
        return results;
    }
}
