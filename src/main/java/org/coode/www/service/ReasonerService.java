package org.coode.www.service;

import com.google.common.collect.ImmutableSet;
import org.coode.www.model.QueryType;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;

import java.util.Set;

@Service
public class ReasonerService {

    public Set<OWLEntity> getResults(final OWLClassExpression owlClassExpression,
                                     final QueryType query,
                                     final OWLReasoner r) {
        switch(query){
            case equivalents: return ImmutableSet.copyOf(r.getEquivalentClasses(owlClassExpression).getEntities());
            case subclasses: return ImmutableSet.copyOf(r.getSubClasses(owlClassExpression, true).getFlattened());
            case descendants: return ImmutableSet.copyOf(r.getSubClasses(owlClassExpression, false).getFlattened());
            case superclasses: return ImmutableSet.copyOf(r.getSuperClasses(owlClassExpression, true).getFlattened());
            case ancestors: return ImmutableSet.copyOf(r.getSuperClasses(owlClassExpression, false).getFlattened());
            case instances: return ImmutableSet.copyOf(r.getInstances(owlClassExpression, false).getFlattened());
        }
        throw new IllegalArgumentException("Unexpected query: " + query);
    }
}
