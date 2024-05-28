package org.ontbrowser.www.service.hierarchy;

import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;

import java.util.Collections;
import java.util.Comparator;
import java.util.Set;
import java.util.stream.Collectors;

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
        if (entity == root) {
            return getRoots(); // Told reasoner fails to return individuals with no asserted type
        }
        return reasoner.getInstances(type, direct).getNodes();
    }

    private Set<Node<OWLNamedIndividual>> getRoots() {
        OWLOntology ont = reasoner.getRootOntology();
        OWLClass owlThing = ont.getOWLOntologyManager().getOWLDataFactory().getOWLThing();
        Set<OWLIndividual> nonRoots = ont.axioms(AxiomType.CLASS_ASSERTION, Imports.INCLUDED)
                .filter(ax -> ax.getClassExpression() != owlThing)
                .map(ax -> ax.getIndividual())
                .collect(Collectors.toSet());

        Set<OWLNamedIndividual> all = ont.getIndividualsInSignature(Imports.INCLUDED);
        all.removeAll(nonRoots);

        return all.stream().map(OWLNamedIndividualNode::new).collect(Collectors.toSet());
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
