package org.ontbrowser.www.service.hierarchy;

import org.apache.commons.lang3.NotImplementedException;
import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.Node;

import java.util.*;
import java.util.stream.Stream;

public class OWLOntologyHierarchyService implements OWLHierarchyService<OWLOntology> {

    private final OWLOntology rootOntology;
    private final Comparator<Tree<OWLOntology>> comparator;

    public OWLOntologyHierarchyService(final OWLOntology rootOntology, Comparator<Tree<OWLOntology>> comparator) {
        this.rootOntology = rootOntology;
        this.comparator = comparator;
    }

    @Override
    public Tree<OWLOntology> getPrunedTree(OWLOntology focus) {
        return getTree(rootOntology, new HashSet<>());
    }

    @Override
    public Tree<OWLOntology> getChildren(OWLOntology base) {
        throw new RuntimeException("Ouch");
    }

    @Override
    public Stream<Node<OWLOntology>> getDescendants(OWLOntology ont) {
        throw new NotImplementedException("Descendants not implemented for ontologies");
    }

    private Tree<OWLOntology> getTree(OWLOntology ont, Set<OWLOntology> visited) {
        if (visited.contains(ont)){ // detect loops
            return new Tree<>(Collections.singleton(ont), Collections.emptyList());
        }
        // TODO cache

        visited.add(ont);
        List<Tree<OWLOntology>> children = ont.getDirectImports().stream()
                .map(o -> getTree(o, visited))
                .sorted(comparator)
                .toList();
        return new Tree<>(Collections.singleton(ont), children);
    }
}
