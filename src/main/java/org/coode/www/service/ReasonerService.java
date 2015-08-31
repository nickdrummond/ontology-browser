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
            case equivalents: return ImmutableSet.<OWLEntity>copyOf(r.getEquivalentClasses(owlClassExpression).getEntities());
            case subclasses: return ImmutableSet.<OWLEntity>copyOf(r.getSubClasses(owlClassExpression, true).getFlattened());
            case descendants: return ImmutableSet.<OWLEntity>copyOf(r.getSubClasses(owlClassExpression, false).getFlattened());
            case superclasses: return ImmutableSet.<OWLEntity>copyOf(r.getSuperClasses(owlClassExpression, true).getFlattened());
            case ancestors: return ImmutableSet.<OWLEntity>copyOf(r.getSuperClasses(owlClassExpression, false).getFlattened());
            case instances: return ImmutableSet.<OWLEntity>copyOf(r.getInstances(owlClassExpression, false).getFlattened());
        }
        throw new IllegalArgumentException("Unexpected query: " + query);
    }
}
