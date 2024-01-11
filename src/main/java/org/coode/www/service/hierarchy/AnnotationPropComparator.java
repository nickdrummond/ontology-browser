package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.coode.www.util.PairwiseOrdering;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

/**
 * Use one annotation property relating individuals as an order for individuals.
 * Cannot think of a use case for this ATM but it reflects the functionality
 * of PropComparator which does the same for ObjectProperty.
 */
public class AnnotationPropComparator implements Comparator<Tree<Relation<OWLAnnotationProperty>>> {

    private final OWLOntology ont;
    private final List<OWLIndividual> order;

    public AnnotationPropComparator(OWLAnnotationProperty property, OWLOntology ont) {
        this.ont = ont;

        PairwiseOrdering<OWLIndividual> orderIndex = new PairwiseOrdering<>();

        // build order indices
        for (OWLAxiom ax: ont.getReferencingAxioms(property, Imports.INCLUDED)) {
            ax.accept(new OWLAxiomVisitor() {
                @Override
                public void visit(OWLAnnotationAssertionAxiom axiom) {
                    Optional<OWLNamedIndividual> subject = getIndividual(axiom.getSubject());
                    Optional<OWLNamedIndividual> value = getIndividual(axiom.getValue());
                    if (subject.isPresent() && value.isPresent()) {
                        orderIndex.add(Arrays.asList(value.get(), subject.get()));
                    }
                }
            });
        }

        this.order = orderIndex.flattened();
    }

    private Optional<OWLNamedIndividual> getIndividual(OWLAnnotationValue value) {
        return value.asIRI().map(this::getIndividual);
    }

    private Optional<OWLNamedIndividual> getIndividual(OWLAnnotationSubject subject) {
        return subject.asIRI().map(this::getIndividual);
    }

    private OWLNamedIndividual getIndividual(IRI iri) {
        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();
        return ont.containsIndividualInSignature(iri) ? df.getOWLNamedIndividual(iri) : null;
    }

    @Override
    public int compare(Tree<Relation<OWLAnnotationProperty>> o1, Tree<Relation<OWLAnnotationProperty>> o2) {
        int index1 = order.indexOf(o1.value.iterator().next().individual());
        int index2 = order.indexOf(o2.value.iterator().next().individual());
        if (index1 == index2) return 0;
        return (index1 < index2) ? -1 : 1;
    }
}
