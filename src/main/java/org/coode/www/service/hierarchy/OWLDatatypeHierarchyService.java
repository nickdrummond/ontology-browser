package org.coode.www.service.hierarchy;

import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLDatatypeNode;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Set;
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
    protected Node<OWLDatatype> topNode() {
        return topNode;
    }

    @Override
    protected Set<Node<OWLDatatype>> subs(OWLDatatype cls) {
        Set<Node<OWLDatatype>> subs = new HashSet<>();
        if (topNode.contains(cls)){
            for (OWLOntology ont : ontologies){
                subs.addAll(ont.getDatatypesInSignature().stream().map(OWLDatatypeNode::new).collect(Collectors.toSet()));
            }
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
