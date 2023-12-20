package org.coode.www.service.hierarchy;

import org.apache.commons.lang3.NotImplementedException;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class OWLOntologyHierarchyService implements OWLHierarchyService<OWLOntology> {

    private final OWLOntology rootOntology;
    private final Comparator<Tree<OWLOntology>> comparator;

    public OWLOntologyHierarchyService(final OWLOntology rootOntology, Comparator<Tree<OWLOntology>> comparator) {
        this.rootOntology = rootOntology;
        this.comparator = comparator;
    }

    @Override
    public Tree<OWLOntology> getPrunedTree(OWLOntology focus) {
        return getTree(rootOntology);
    }

    @Override
    public Tree<OWLOntology> getChildren(OWLOntology base) {
        throw new RuntimeException("Ouch");
    }

    @Override
    public Tree<OWLOntology> getSubtree(OWLOntology root) {
        throw new NotImplementedException("Not yet");
    }

    private Tree<OWLOntology> getTree(OWLOntology ont) {
        List<Tree<OWLOntology>> children = ont.getDirectImports().stream()
                .map(this::getTree)
                .sorted(comparator)
                .collect(Collectors.toList());
        return new Tree<>(Collections.singleton(ont), children);
    }
}
