package org.ontbrowser.www.service.hierarchy;

import org.apache.commons.lang3.NotImplementedException;
import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;

import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.stream.Stream;

public class IndividualsByTypeHierarchyService
        extends AbstractOWLHierarchyService<OWLNamedIndividual> {

    private OWLClass type;
    private OWLReasoner reasoner;

    private OWLNamedIndividual root;
    private final boolean direct;

    public IndividualsByTypeHierarchyService(Comparator<? super Tree<OWLNamedIndividual>> comparator) {
        this(true, comparator);
    }

    public IndividualsByTypeHierarchyService(boolean direct, Comparator<? super Tree<OWLNamedIndividual>> comparator) {
        super(comparator);
        this.direct = direct;
    }

    @Override
    protected Node<OWLNamedIndividual> topNode() {
        return new OWLNamedIndividualNode(root);
    }

    @Override
    protected Set<Node<OWLNamedIndividual>> subs(OWLNamedIndividual entity) {
        return reasoner.getInstances(type, direct).getNodes();
    }

    @Override
    protected Set<Node<OWLNamedIndividual>> ancestors(OWLNamedIndividual entity) {
        if (entity == root) {
            return Collections.emptySet();
        }
        return Collections.singleton(new OWLNamedIndividualNode(root));
    }

    @Override
    protected Node<OWLNamedIndividual> equivs(OWLNamedIndividual entity) {
        return reasoner.getSameIndividuals(entity);
    }

    public IndividualsByTypeHierarchyService withType(final OWLClass type,
                                                      final OWLReasoner reasoner) {
        this.type = type;
        this.reasoner = reasoner;
        // Pun to avoid the tree having to be generic
        this.root = reasoner.getRootOntology().getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(type.getIRI());

        return this;
    }
}
