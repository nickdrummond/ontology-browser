package org.ontbrowser.www.feature.graph;

import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

// Create a graph from a set of axioms
public class AxiomGraphBuilder implements GraphBuilder {

    private static final Logger log = LoggerFactory.getLogger(AxiomGraphBuilder.class);

    private Set<OWLAxiom> axioms = new HashSet<>();
    private List<Predicate<Graph.Edge>> vetos = new ArrayList<>();

    public void addAxioms(Set<OWLAxiom> axioms) {
        this.axioms.addAll(axioms);
    }

    private boolean veto(Graph.Edge edge) {
        return vetos.stream().anyMatch(predicate -> predicate.test(edge));
    }

    public Graph build() {
        var edgesFromAxioms = new HashSet<Graph.Edge>();
        var edgeCreator = new EdgeCreator();
        for (var axiom : axioms) {
            edgesFromAxioms.addAll(axiom.accept(edgeCreator).stream()
                    .filter(edge -> !veto(edge))
                    .collect(Collectors.toSet()));
        }
        return new Graph(edgesFromAxioms);
    }

    public void addVeto(Predicate<Graph.Edge> veto) {
        vetos.add(veto);
    }

    static class EdgeCreator implements OWLAxiomVisitorEx<Set<Graph.Edge>> {
        @Override
        public Set<Graph.Edge> visit(OWLObjectPropertyAssertionAxiom axiom) {
            if (axiom.getProperty().isNamed()) {
                return Set.of(new Graph.Edge(axiom.getSubject(), axiom.getProperty().asOWLObjectProperty(), axiom.getObject()));
            }
            else {
                return Set.of(new Graph.Edge(axiom.getObject(), axiom.getProperty().getNamedProperty(), axiom.getSubject()));
            }
        }

        @Override
        public Set<Graph.Edge> visit(OWLDataPropertyAssertionAxiom axiom) {
            if (axiom.getProperty().isNamed()) {
                return Set.of(new Graph.Edge(axiom.getSubject(), axiom.getProperty().asOWLDataProperty(), axiom.getObject()));
            }
            else {
                log.warn("Unexpected data property assertion: {}", axiom);
                return Set.of();
            }
        }

        @Override
        public <T> Set<Graph.Edge> doDefault(T object) {
            return Set.of();
        }
    }
}

