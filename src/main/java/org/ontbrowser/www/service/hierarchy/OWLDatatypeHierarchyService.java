package org.ontbrowser.www.service.hierarchy;

import com.google.common.collect.Sets;
import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLDatatypeNode;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class OWLDatatypeHierarchyService extends AbstractOWLHierarchyService<OWLDatatype> {

    private final OWLDatatypeNode topNode;
    private final OWLOntology ont;

    public OWLDatatypeHierarchyService(final OWLOntology ont,
                                       final Comparator<? super Tree<OWLDatatype>> comparator) {
        super(comparator);
        this.ont = ont;
        this.topNode = new OWLDatatypeNode(ont.getOWLOntologyManager().getOWLDataFactory().getTopDatatype());
    }

    @Override
    protected Node<OWLDatatype> topNode() {
        return topNode;
    }

    @Override
    protected Set<Node<OWLDatatype>> subs(OWLDatatype cls) {
        Set<Node<OWLDatatype>> subs = new HashSet<>();
        if (topNode.contains(cls)){
            subs.addAll(ont.getDatatypesInSignature(Imports.INCLUDED).stream()
                    .map(OWLDatatypeNode::new).collect(Collectors.toSet()));
        }
        return subs;
    }

    @Override
    protected Set<Node<OWLDatatype>> ancestors(OWLDatatype cls) {
        if (!topNode.contains(cls)){
            return Sets.<Node<OWLDatatype>>newHashSet(topNode);
        }
        return Collections.emptySet();
    }

    @Override
    protected Node<OWLDatatype> equivs(OWLDatatype cls) {
        return new OWLDatatypeNode(cls);
    }
}
