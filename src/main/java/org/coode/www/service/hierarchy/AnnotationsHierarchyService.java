package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.*;
import java.util.stream.Collectors;

public class AnnotationsHierarchyService extends AbstractRelationsHierarchyService<OWLAnnotationProperty> {

    private List<OWLNamedIndividual> roots;

    private List<OWLNamedIndividual> nonRoots = new ArrayList<>();

    private LinkedHashMap<OWLNamedIndividual, List<Relation<OWLAnnotationProperty>>> nodes = new LinkedHashMap<>();

    private LinkedHashMap<OWLNamedIndividual, List<Relation<OWLAnnotationProperty>>> reverseNodes = new LinkedHashMap<>();

    public AnnotationsHierarchyService() {
        super();
    }

    public AnnotationsHierarchyService(final Comparator<? super Tree<Relation<OWLAnnotationProperty>>> comparator) {
        super(comparator);
    }

    public OWLAnnotationProperty getProperty() {
        return property;
    }

    public boolean isInverse() {
        return inverse;
    }

    @Override
    protected Set<Relation<OWLAnnotationProperty>> topNode() {
        return Set.of(new Relation<>(property, root));
    }

    @Override
    protected Set<Set<Relation<OWLAnnotationProperty>>> subs(Relation<OWLAnnotationProperty> rel) {
        buildIfNecessary();

        // fake roots
        if (rel.individual().equals(root)) return wrap(rootsAsRelations());

        return wrap(nodes.get(rel.individual()));
    }

    // TODO should cache
    private List<Relation<OWLAnnotationProperty>> rootsAsRelations() {
        return roots.stream().map(r -> new Relation<>(property, r)).toList();
    }

    @Override
    protected Set<Set<Relation<OWLAnnotationProperty>>> ancestors(Relation<OWLAnnotationProperty> rel) {
        if (rel.individual().equals(root)) return Collections.emptySet();

        buildIfNecessary();

        if (roots.contains(rel.individual())) return wrap(rootsAsRelations());

        Set<Set<Relation<OWLAnnotationProperty>>> ancestors = new HashSet<>();
        for (Relation<OWLAnnotationProperty> parent : reverseNodes.get(rel.individual())) {
            ancestors.add(Set.of(parent));
            ancestors.addAll(ancestors(parent));
        }
        return ancestors;
    }

    @Override
    protected Set<Relation<OWLAnnotationProperty>> equivs(Relation<OWLAnnotationProperty> ind) {
        buildIfNecessary();
        return Set.of(ind);
        // TODO
    }

    public boolean treeContains(Relation<OWLAnnotationProperty> rel) {
        buildIfNecessary();
        return nodes.containsKey(rel.individual()) || reverseNodes.containsKey(rel.individual());
    }

    @Override
    protected boolean isBottomNode(Set<Relation<OWLAnnotationProperty>> subNode) {
        return getRepresentativeElement(subNode).individual().equals(root);
    }

    @Override
    protected Relation<OWLAnnotationProperty> getRepresentativeElement(Set<Relation<OWLAnnotationProperty>> node) {
        return node.iterator().next();
    }

    public void buildIfNecessary() {
        if (roots == null) {
            for (OWLAxiom ax : ont.getReferencingAxioms(property, Imports.INCLUDED)) {
                ax.accept(new OWLAxiomVisitor() {
                    @Override
                    public void visit(OWLAnnotationAssertionAxiom axiom) {
                        Optional<OWLNamedIndividual> subject = getIndividual(axiom.getSubject());
                        Optional<OWLNamedIndividual> object = getIndividual(axiom.getValue());
                        if (subject.isPresent() && object.isPresent()){
                            if (inverse) {
                                insert(property, object.get(), subject.get());
                            } else {
                                insert(property, subject.get(), object.get());
                            }
                        }
                    }
                });
            }

            roots = new ArrayList<>(nodes.keySet());
            roots.removeAll(nonRoots);
        }
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

    private void insert(OWLAnnotationProperty prop, OWLNamedIndividual parent, OWLNamedIndividual child) {
        insert(prop, parent, child, nodes);
        insert(prop, child, parent, reverseNodes);
        nonRoots.add(child); // so we can determine roots
    }

    private void insert(OWLAnnotationProperty prop, OWLNamedIndividual parent,
                        OWLNamedIndividual child, Map<OWLNamedIndividual, List<Relation<OWLAnnotationProperty>>> map) {
        List<Relation<OWLAnnotationProperty>> children = map.computeIfAbsent(parent, k -> new ArrayList<>());
        children.add(new Relation<>(prop, child));
    }

    private Set<Set<Relation<OWLAnnotationProperty>>> wrap(List<Relation<OWLAnnotationProperty>> relations) {
        if (relations == null) {
            return Collections.emptySet();
        }
        return relations.stream().map(Set::of).collect(Collectors.toSet());
    }
}
