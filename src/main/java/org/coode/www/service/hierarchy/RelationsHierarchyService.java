package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.*;
import java.util.stream.Collectors;

public class RelationsHierarchyService extends AbstractRelationsHierarchyService<OWLObjectProperty> {

    private List<OWLNamedIndividual> roots;

    private final List<OWLNamedIndividual> nonRoots = new ArrayList<>();

    private final LinkedHashMap<OWLNamedIndividual, List<Relation<OWLObjectProperty>>> nodes = new LinkedHashMap<>();

    private final LinkedHashMap<OWLNamedIndividual, List<Relation<OWLObjectProperty>>> reverseNodes = new LinkedHashMap<>();

    public RelationsHierarchyService() {
        super();
    }

    public RelationsHierarchyService(final Comparator<? super Tree<Relation<OWLObjectProperty>>> comparator) {
        super(comparator);
    }

    @Override
    protected Set<Relation<OWLObjectProperty>> topNode() {
        return Set.of(new Relation<>(property, root));
    }

    @Override
    protected Set<Set<Relation<OWLObjectProperty>>> subs(Relation<OWLObjectProperty> rel) {
        buildIfNecessary();

        // fake roots
        if (rel.individual().equals(root)) return wrap(rootsAsRelations());

        return wrap(nodes.get(rel.individual()));
    }

    // TODO should cache
    private List<Relation<OWLObjectProperty>> rootsAsRelations() {
        return roots.stream().map(r -> new Relation<>(property, r)).toList();
    }

    @Override
    protected Set<Set<Relation<OWLObjectProperty>>> ancestors(Relation<OWLObjectProperty> rel) {
        if (rel.individual().equals(root)) return Collections.emptySet();

        buildIfNecessary();

        if (roots.contains(rel.individual())) return wrap(rootsAsRelations());

        Set<Set<Relation<OWLObjectProperty>>> ancestors = new HashSet<>();
        for (Relation<OWLObjectProperty> parent : reverseNodes.get(rel.individual())) {
            ancestors.add(Set.of(parent));
            ancestors.addAll(ancestors(parent));
        }
        return ancestors;
    }

    @Override
    protected Set<Relation<OWLObjectProperty>> equivs(Relation<OWLObjectProperty> ind) {
        buildIfNecessary();
        return Set.of(ind);
        // TODO
    }

    public boolean treeContains(Relation<OWLObjectProperty> rel) {
        buildIfNecessary();
        return nodes.containsKey(rel.individual()) || reverseNodes.containsKey(rel.individual());
    }

    @Override
    protected boolean isBottomNode(Set<Relation<OWLObjectProperty>> subNode) {
        return getRepresentativeElement(subNode).individual().equals(root);
    }

    @Override
    protected Relation<OWLObjectProperty> getRepresentativeElement(Set<Relation<OWLObjectProperty>> node) {
        return node.iterator().next();
    }

    public void buildIfNecessary() {
        if (roots == null) {
            for (OWLObjectProperty p : properties) {
                for (OWLAxiom ax : ont.getReferencingAxioms(p, Imports.INCLUDED)) {
                    ax.accept(new OWLAxiomVisitor() {
                        @Override
                        public void visit(OWLObjectPropertyAssertionAxiom axiom) {
                            if (inverse) {
                                // TODO should this be inv(p)?
                                insert(p, axiom.getObject().asOWLNamedIndividual(), axiom.getSubject().asOWLNamedIndividual());
                            } else {
                                insert(p, axiom.getSubject().asOWLNamedIndividual(), axiom.getObject().asOWLNamedIndividual());
                            }
                        }
                    });
                }
            }

            roots = new ArrayList<>(nodes.keySet());
            roots.removeAll(nonRoots);
        }
    }

    private void insert(OWLObjectProperty prop, OWLNamedIndividual parent, OWLNamedIndividual child) {
        insert(prop, parent, child, nodes);
        insert(prop, child, parent, reverseNodes);
        nonRoots.add(child); // so we can determine roots
    }

    private void insert(OWLObjectProperty prop, OWLNamedIndividual parent, OWLNamedIndividual child, Map<OWLNamedIndividual, List<Relation<OWLObjectProperty>>> map) {
        List<Relation<OWLObjectProperty>> children = map.computeIfAbsent(parent, k -> new ArrayList<>());
        children.add(new Relation<>(prop, child));
    }

    private Set<Set<Relation<OWLObjectProperty>>> wrap(List<Relation<OWLObjectProperty>> relations) {
        if (relations == null) {
            return Collections.emptySet();
        }
        return relations.stream().map(Set::of).collect(Collectors.toSet());
    }
}
