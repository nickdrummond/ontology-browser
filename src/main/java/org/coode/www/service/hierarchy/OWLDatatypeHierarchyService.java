package org.coode.www.service.hierarchy;

import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLDatatypeNode;
import org.semanticweb.owlapi.reasoner.impl.OWLDatatypeNodeSet;

import java.util.*;
import java.util.stream.Collectors;

public class OWLDatatypeHierarchyService extends AbstractOWLHierarchyService<OWLDatatype> {

    private final OWLDatatypeNode topNode;
    private final Set<OWLOntology> ontologies;

    public OWLDatatypeHierarchyService(final OWLDataFactory dataFactory,
                                       final Set<OWLOntology> ontologies,
                                       final Comparator<? super Tree<OWLDatatype>> comparator) {
        super(comparator);
        this.ontologies = ontologies;
        this.topNode = new OWLDatatypeNode(dataFactory.getTopDatatype());
    }

    @Override
    protected NodeSet<OWLDatatype> newNodeSet(Set<Node<OWLDatatype>> nodes) {
        return new OWLDatatypeNodeSet(nodes);
    }

    @Override
    protected Node<OWLDatatype> topNode() {
        return topNode;
    }

    @Override
    protected NodeSet<OWLDatatype> subs(OWLDatatype cls) {
        Set<Node<OWLDatatype>> subs = new HashSet<>();
        if (topNode.contains(cls)){
            for (OWLOntology ont : ontologies){
                subs.addAll(ont.getDatatypesInSignature().stream().map(OWLDatatypeNode::new).collect(Collectors.toSet()));
            }
        }
        return newNodeSet(subs);
    }

    @Override
    protected NodeSet<OWLDatatype> ancestors(OWLDatatype cls) {
        if (!topNode.contains(cls)){
            return newNodeSet(Sets.<Node<OWLDatatype>>newHashSet(topNode));
        }
        return newNodeSet(Collections.emptySet());
    }

    @Override
    protected Node<OWLDatatype> equivs(OWLDatatype cls) {
        return new OWLDatatypeNode(cls);
    }
}
