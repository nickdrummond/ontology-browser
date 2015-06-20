package org.coode.www.service;

import org.coode.www.QueryType;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.Set;

@Service
public class ReasonerService {

    public Set<OWLEntity> getResults(final OWLClassExpression owlClassExpression,
                                     final QueryType query,
                                     final OWLReasoner r) {
        Set<OWLEntity> results = new HashSet<OWLEntity>();
        switch(query){
            case equivalents: results.addAll(r.getEquivalentClasses(owlClassExpression).getEntities()); break;
            case subclasses: results.addAll(r.getSubClasses(owlClassExpression, true).getFlattened()); break;
            case descendants: results.addAll(r.getSubClasses(owlClassExpression, false).getFlattened()); break;
            case superclasses: results.addAll(r.getSuperClasses(owlClassExpression, true).getFlattened()); break;
            case ancestors: results.addAll(r.getSuperClasses(owlClassExpression, false).getFlattened()); break;
            case instances: results.addAll(r.getInstances(owlClassExpression, false).getFlattened()); break;
        }
        return results;
    }
}
