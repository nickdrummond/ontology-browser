package org.ontbrowser.www.feature.graph;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;

public record Graph(Set<Edge> edges) {

    public List<Edge> getEdgesWithSubject(OWLObject entity) {
        return getEdgesMatching(edge -> edge.subject.equals(entity));
    }

    public List<Edge> getEdgesWithPredicate(OWLProperty entity) {
        return getEdgesMatching(edge -> edge.predicate.equals(entity));
    }

    public List<Edge> getEdgesWithObject(OWLObject entity) {
        return getEdgesMatching(edge -> edge.object.equals(entity));
    }

    public List<Edge> getEdgesMatching(Predicate<? super Edge> filter) {
        return edges.stream().filter(filter).sorted(Comparator.comparing(e -> e.predicate)).toList();
    }

    public record Edge(OWLObject subject, OWLProperty predicate, OWLObject object) {
    }
}
