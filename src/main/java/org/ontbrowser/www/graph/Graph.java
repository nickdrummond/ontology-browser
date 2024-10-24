package org.ontbrowser.www.graph;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

public record Graph(Set<Edge> edges) {

    public List<Edge> getEdgesWithSubject(OWLEntity entity) {
        return getEdgesMatching(edge -> edge.subject.equals(entity));
    }

    public List<Edge> getEdgesWithPredicate(OWLProperty entity) {
        return getEdgesMatching(edge -> edge.predicate.equals(entity));
    }

    public List<Edge> getEdgesWithObject(OWLEntity entity) {
        return getEdgesMatching(edge -> edge.object.equals(entity));
    }

    public List<Edge> getEdgesMatching(Predicate<? super Edge> filter) {
        return edges.stream().filter(filter).sorted(Comparator.comparing(e -> e.predicate)).toList();
    }

    public record Edge(OWLEntity subject, OWLProperty predicate, OWLEntity object) {
    }
}
