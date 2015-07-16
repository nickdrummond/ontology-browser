package org.coode.www.service.hierarchy;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.OWLClassNodeSet;

import java.util.*;

public class OWLIndividualsByTypeHierarchyService implements OWLHierarchyService<OWLEntity> {

    private final OWLReasoner reasoner;
    private final Comparator<? super Tree<OWLEntity>> comparator;

    public OWLIndividualsByTypeHierarchyService(final OWLReasoner reasoner,
                                                final Comparator<? super Tree<OWLEntity>> comparator) {
        this.reasoner = reasoner;
        this.comparator = comparator;
    }

    public Tree<OWLEntity> getPrunedTree(final OWLEntity entity) {
        NodeSet<OWLClass> expandedClasses;

        if (entity.isOWLClass()) {
            expandedClasses = new OWLClassNodeSet(reasoner.getEquivalentClasses(entity.asOWLClass()));
        }
        else if (entity.isOWLNamedIndividual()) {
            expandedClasses = reasoner.getTypes(entity.asOWLNamedIndividual(), true);
        }
        else {
            throw new IllegalArgumentException("Hierarchy Service cannot handle " + entity);
        }
        System.out.println(expandedClasses);
        return new Tree<>(toEntity(reasoner.getTopClassNode()), getImplicitRoots(expandedClasses));
    }

    public List<Tree<OWLEntity>> getImplicitRoots(NodeSet<OWLClass> expandedClasses) {
        Set<OWLNamedIndividual> inds = reasoner.getRootOntology().getIndividualsInSignature(Imports.INCLUDED);
        List<Tree<OWLEntity>> emptyTree = Collections.<Tree<OWLEntity>>emptyList();
        Map<Node<OWLClass>, Tree<OWLEntity>> types = Maps.newHashMap();
        for (OWLNamedIndividual ind : inds) {
            for (Node<OWLClass> type : reasoner.getTypes(ind, true)) {
                if (!types.containsKey(type)) {
                    List<Tree<OWLEntity>> instanceTrees;
                    if (expandedClasses.containsEntity(type.getRepresentativeElement())) {
                        instanceTrees = Lists.newArrayList();
                        for (Node<OWLNamedIndividual> instance : reasoner.getInstances(type.getRepresentativeElement(), true)) {
                            instanceTrees.add(new Tree<>(toEntity(instance), emptyTree));
                        }
                        instanceTrees.sort(comparator);
                    }
                    else {
                        instanceTrees = emptyTree;
                    }
                    types.put(type, new Tree<>(toEntity(type), instanceTrees));
                }
            }
        }
        List<Tree<OWLEntity>> sorted = Lists.newArrayList(types.values());
        sorted.sort(comparator);
        return sorted;
    }

    // Easy create a more generic type iterable
    private Iterable<OWLEntity> toEntity(Node<? extends OWLEntity> node) {
        return Lists.newArrayList(node);
    }
}