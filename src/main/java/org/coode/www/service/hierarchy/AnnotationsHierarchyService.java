package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.OWLNamedIndividualNode;

import java.util.*;
import java.util.stream.Collectors;

public class AnnotationsHierarchyService extends AbstractOWLHierarchyService<OWLNamedIndividual> {

    private final OWLAnnotationProperty property;

    private final boolean inverse;

    private final OWLOntology ont;

    private final OWLNamedIndividual root;

    private List<OWLNamedIndividual> roots;

    private List<OWLNamedIndividual> nonRoots = new ArrayList<>();

    private LinkedHashMap<OWLNamedIndividual, List<OWLNamedIndividual>> nodes = new LinkedHashMap<>();

    private LinkedHashMap<OWLNamedIndividual, List<OWLNamedIndividual>> reverseNodes = new LinkedHashMap<>();

    public AnnotationsHierarchyService(final OWLAnnotationProperty property,
                                       final OWLOntology ont,
                                       final boolean inverse,
                                       final Comparator<? super Tree<OWLNamedIndividual>> comparator) {
        super(comparator);
        this.property = property;
        this.ont = ont;
        this.inverse = inverse;

        // dummy root - pun the property to avoid the generic tree
        this.root = ont.getOWLOntologyManager().getOWLDataFactory().getOWLNamedIndividual(property.getIRI());
    }

    public OWLAnnotationProperty getProperty() {
        return property;
    }

    public boolean isInverse() {
        return inverse;
    }

    @Override
    protected Node<OWLNamedIndividual> topNode() {
        return new OWLNamedIndividualNode(root);
    }

    @Override
    protected Set<Node<OWLNamedIndividual>> subs(OWLNamedIndividual ind) {
        buildIfNecessary();

        if (ind.equals(root)) return wrap(roots);

        return wrap(nodes.get(ind));
    }

    @Override
    protected Set<Node<OWLNamedIndividual>> ancestors(OWLNamedIndividual ind) {
        if (ind.equals(root)) return Collections.emptySet();

        buildIfNecessary();

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
        buildIfNecessary();
        return new OWLNamedIndividualNode(ind); // TODO
    }

    public boolean treeContains(OWLNamedIndividual ind) {
        buildIfNecessary();
        return nodes.containsKey(ind) || reverseNodes.containsKey(ind);
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
                                insert(object.get(), subject.get());
                            } else {

                                insert(subject.get(), object.get());
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

    private Set<Node<OWLNamedIndividual>> wrap(List<OWLNamedIndividual> inds) {
        if (inds == null) {
            return Collections.emptySet();
        }
        return inds.stream().map(OWLNamedIndividualNode::new).collect(Collectors.toSet());
    }
}
