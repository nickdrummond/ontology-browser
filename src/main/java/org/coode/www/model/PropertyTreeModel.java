package org.coode.www.model;

import org.coode.www.util.PairwiseOrdering;
import org.semanticweb.owlapi.model.*;

import java.util.*;
import java.util.stream.Collectors;

public class PropertyTreeModel {

    private OWLObjectProperty root;

    private List<OWLIndividual> roots;

    private ArrayList<OWLIndividual> nonRoots = new ArrayList<>();

    private LinkedHashMap<OWLIndividual, List<OWLIndividual>> nodes = new LinkedHashMap<>();

    public PropertyTreeModel(final OWLObjectProperty childRelation,
                             final Set<OWLOntology> onts,
                             final boolean inverse,
                             final OWLObjectProperty orderByProperty) {
        // TODO rewrite with streams
        for (OWLOntology ont : onts) {
            for (OWLAxiom ax: ont.getReferencingAxioms(childRelation)) {
                ax.accept(new OWLAxiomVisitor() {
                    @Override
                    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                        if (inverse) {
                            insert(axiom.getObject(), axiom.getSubject());
                        }
                        else {
                            insert(axiom.getSubject(), axiom.getObject());
                        }
                    }
                });
            }
        }

        root = childRelation;
        roots = new ArrayList<>(nodes.keySet());
        roots.removeAll(nonRoots);

        // Sort based on orderByProperty
        if (orderByProperty != null) {
            System.out.println("sorting...");

            /*
             * (d, e) = (d, e)
             * (b, c) = (d, e), (b, c)
             * (c, d) = (b, c, d, e) ?? place after entry where key = value, then move any other kv where v = k
             * must move in pairs
             * what about forks?
             * (k, d) = (b, c), (c, d), (k, d), (d, e)
             *
             * TODO extract this into a testable thing
             */

            PairwiseOrdering<OWLIndividual> orderIndex = new PairwiseOrdering<>();
            // build order indices
            for (OWLOntology ont : onts) {
                for (OWLAxiom ax: ont.getReferencingAxioms(orderByProperty)) {
                    ax.accept(new OWLAxiomVisitor() {
                        @Override
                        public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                            orderIndex.add(Arrays.asList(axiom.getObject(), axiom.getSubject()));
                        }
                    });
                }
            }
            List<OWLIndividual> order = orderIndex.flattened();
            Comparator<? super OWLIndividual> sorter = (Comparator<OWLIndividual>) (i1, i2) -> {
                int o1 = order.indexOf(i1);
                int o2 = order.indexOf(i2);
                if (o1 == o2) return 0;
                return (o1 < o2) ? -1 : 1;
            };

            // rework the indices
            for (Map.Entry<OWLIndividual, List<OWLIndividual>> entry : nodes.entrySet()) {
                List<OWLIndividual> children = entry.getValue();
                if (children.size() > 1) { // only sort if necessary
                    nodes.put(entry.getKey(), children.stream().sorted(sorter).collect(Collectors.toList()));
                }
            }
        }
    }

    private void insert(OWLIndividual parent, OWLIndividual child) {
        List<OWLIndividual> children = nodes.get(parent);
        if (children == null) {
            children = new ArrayList<>();
            nodes.put(parent, children);
        }
        nonRoots.add(child); // so we can determine roots
        children.add(child);
    }

    public Object getRoot() {
        return root;
    }

    public Object getChild(Object parent, int index) {
        if (parent == root) {
            return roots.get(index);
        }
        return nodes.get((OWLIndividual) parent).get(index);
    }

    public int getChildCount(Object parent) {
        if (parent == root) {
            return roots.size();
        }
        return nodes.get((OWLIndividual) parent).size();
    }

    public boolean isLeaf(Object node) {
        if (node == root) {
            return nodes.isEmpty();
        }
        return !nodes.containsKey(node);
    }

    public int getIndexOfChild(Object parent, Object child) {
        if (parent == root) {
            return roots.indexOf(child);
        }
        return nodes.get((OWLIndividual) parent).indexOf(child);
    }

}
