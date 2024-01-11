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

public class OWLDatatypeHierarchyService extends AbstractHierarchyService<OWLDatatype> {

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
    protected boolean isBottomNode(Set<OWLDatatype> subNode) {
        return false;
    }

    @Override
    protected OWLDatatype getRepresentativeElement(Set<OWLDatatype> node) {
        return node.iterator().next();
    }

    @Override
    protected Set<OWLDatatype> topNode() {
        return topNode.getEntities();
    }

    @Override
    protected Set<Set<OWLDatatype>> subs(OWLDatatype cls) {
        Set<Set<OWLDatatype>> subs = new HashSet<>();
        if (topNode.contains(cls)){
            for (OWLOntology ont : ontologies){
                subs.addAll(ont.getDatatypesInSignature().stream().map(Set::of).collect(Collectors.toSet()));
            }
        }
        return subs;
    }

    @Override
    protected Set<Set<OWLDatatype>> ancestors(OWLDatatype cls) {
        if (!topNode.contains(cls)){
            return Set.of(topNode.getEntities());
        }
        return Collections.emptySet();
    }

    @Override
    protected Set<OWLDatatype> equivs(OWLDatatype cls) {
        return Set.of(cls);
    }
}
