package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.coode.www.util.PairwiseOrdering;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RelationsHierarchyService extends AbstractOWLHierarchyService<OWLNamedIndividual> {

    private final OWLObjectProperty property;
    private final OWLNamedIndividual root;

    private List<OWLNamedIndividual> roots;

    private ArrayList<OWLNamedIndividual> nonRoots = new ArrayList<>();

    private LinkedHashMap<OWLNamedIndividual, List<OWLNamedIndividual>> nodes = new LinkedHashMap<>();

    private LinkedHashMap<OWLNamedIndividual, List<OWLNamedIndividual>> reverseNodes = new LinkedHashMap<>();

    public RelationsHierarchyService(final OWLObjectProperty property,
                                     final Set<OWLOntology> onts,
                                     final boolean inverse,
                                     final @Nullable OWLObjectProperty orderByProperty, // TODO push into comparator
                                     final Comparator<? super Tree<OWLNamedIndividual>> comparator) {
        super(comparator);
        this.property = property;
        // pun the property to avoid the generic tree
        this.root = onts.iterator().next().getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(property.getIRI());

        // TODO rewrite with streams
        for (OWLOntology ont : onts) {
            for (OWLAxiom ax: ont.getReferencingAxioms(property)) {
                ax.accept(new OWLAxiomVisitor() {
                    @Override
                    public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                        if (inverse) {
                            insert(axiom.getObject().asOWLNamedIndividual(), axiom.getSubject().asOWLNamedIndividual());
                        }
                        else {
                            insert(axiom.getSubject().asOWLNamedIndividual(), axiom.getObject().asOWLNamedIndividual());
                        }
                    }
                });
            }
        }

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
            for (Map.Entry<OWLNamedIndividual, List<OWLNamedIndividual>> entry : nodes.entrySet()) {
                List<OWLNamedIndividual> children = entry.getValue();
                if (children.size() > 1) { // only sort if necessary
                    nodes.put(entry.getKey(), children.stream().sorted(sorter).collect(Collectors.toList()));
                }
            }
        }
    }

    private void insert(OWLNamedIndividual parent, OWLNamedIndividual child) {
        insert(parent, child, nodes);
        insert(child, parent, reverseNodes);
        nonRoots.add(child); // so we can determine roots
    }

    private void insert(OWLNamedIndividual parent, OWLNamedIndividual child, Map<OWLNamedIndividual, List<OWLNamedIndividual>> map) {
        List<OWLNamedIndividual> children = map.get(parent);
        if (children == null) {
            children = new ArrayList<>();
            map.put(parent, children);
        }
        children.add(child);
    }

    @Override
    protected Node<OWLNamedIndividual> topNode() {
        return new OWLNamedIndividualNode(root);
    }

    @Override
    protected Set<Node<OWLNamedIndividual>> subs(OWLNamedIndividual ind) {
        if (ind.equals(root)) {
            return wrap(roots);
        }
        return wrap(nodes.get(ind));
    }

    private Set<Node<OWLNamedIndividual>> wrap(List<OWLNamedIndividual> inds) {
        if (inds == null) {
            return Collections.emptySet();
        }
        return inds.stream().map(OWLNamedIndividualNode::new).collect(Collectors.toSet());
    }

    @Override
    protected Set<Node<OWLNamedIndividual>> ancestors(OWLNamedIndividual ind) {
        if (ind.equals(root)) return Collections.emptySet();
        if (roots.contains(ind)) return Collections.singleton(new OWLNamedIndividualNode(root));
        Set<Node<OWLNamedIndividual>> ancestors = new HashSet<>();
        for (OWLNamedIndividual parent : reverseNodes.get(ind)) {
            ancestors.add(new OWLNamedIndividualNode(parent));
            ancestors.addAll(ancestors(parent));
        }
        return ancestors;
    }

    @Override
    protected Node<OWLNamedIndividual> equivs(OWLNamedIndividual ind) {
        return new OWLNamedIndividualNode(ind); // TODO
    }
}
