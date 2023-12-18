package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.coode.www.util.PairwiseOrdering;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

public class PropComparator implements Comparator<Tree<OWLNamedIndividual>> {

    private List<OWLIndividual> order = null;

    public PropComparator(OWLObjectProperty property, OWLOntology ont) {
        PairwiseOrdering<OWLIndividual> orderIndex = new PairwiseOrdering<>();

        // build order indices
        for (OWLAxiom ax: ont.getReferencingAxioms(property, Imports.INCLUDED)) {
            ax.accept(new OWLAxiomVisitor() {
                @Override
                public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                    orderIndex.add(Arrays.asList(axiom.getObject(), axiom.getSubject()));
                }
            });
        }

        order = orderIndex.flattened();
    }

    @Override
    public int compare(Tree<OWLNamedIndividual> o1, Tree<OWLNamedIndividual> o2) {
        int index1 = order.indexOf(o1.value.iterator().next());
        int index2 = order.indexOf(o2.value.iterator().next());
        if (index1 == index2) return 0;
        return (index1 < index2) ? -1 : 1;
    }
}
