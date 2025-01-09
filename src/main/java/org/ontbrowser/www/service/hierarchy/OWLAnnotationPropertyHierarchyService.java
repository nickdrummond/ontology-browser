package org.ontbrowser.www.service.hierarchy;

import com.google.common.collect.Streams;
import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.DefaultNode;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class OWLAnnotationPropertyHierarchyService extends AbstractOWLHierarchyService<OWLAnnotationProperty> {

    private final OWLOntology ont;
    private final OWLAnnotationProperty dummyroot;
    public final AnnotationPropertyNode dummyRootNode;

    public OWLAnnotationPropertyHierarchyService(
            final OWLOntology ont,
            final Comparator<? super Tree<OWLAnnotationProperty>> comparator) {
        super(comparator);
        this.ont = ont;
        dummyroot = ont.getOWLOntologyManager().getOWLDataFactory().getOWLAnnotationProperty("dummyroot");
        dummyRootNode = new AnnotationPropertyNode(dummyroot);
    }

    @Override
    protected Node<OWLAnnotationProperty> topNode() {
        return dummyRootNode;
    }

    @Override
    protected Set<Node<OWLAnnotationProperty>> subs(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return getRoots();
        }
        return ont.axioms(AxiomType.SUB_ANNOTATION_PROPERTY_OF, Imports.INCLUDED)
                .filter(ax -> ax.getSuperProperty() == prop)
                .map(ax -> new AnnotationPropertyNode(ax.getSubProperty()))
                .collect(Collectors.toSet());
    }

    private Set<Node<OWLAnnotationProperty>> getRoots() {
        Set<OWLAnnotationProperty> nonRoots = ont.axioms(AxiomType.SUB_ANNOTATION_PROPERTY_OF, Imports.INCLUDED)
                .map(ax -> ax.getSubProperty())
                .collect(Collectors.toSet());

        Set<OWLAnnotationProperty> all = ont.getAnnotationPropertiesInSignature(Imports.INCLUDED);
        all.removeAll(nonRoots);

        return all.stream().map(AnnotationPropertyNode::new).collect(Collectors.toSet());
    }

    @Override
    protected Set<Node<OWLAnnotationProperty>> ancestors(OWLAnnotationProperty prop) {
        return getAncestors(prop).collect(Collectors.toSet());
    }

    protected Stream<Node<OWLAnnotationProperty>> getAncestors(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return Stream.empty();
        }
        Set<Node<OWLAnnotationProperty>> parents = parents(prop);
        Stream<Node<OWLAnnotationProperty>> recursive = parents.stream().flatMap(parent -> getAncestors(parent.getRepresentativeElement()));
        return Streams.concat(
                Stream.of(dummyRootNode),
                parents.stream(),
                recursive);
    }

    protected Set<Node<OWLAnnotationProperty>> parents(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return Collections.emptySet();
        }
        Set<Node<OWLAnnotationProperty>> parents = ont.axioms(AxiomType.SUB_ANNOTATION_PROPERTY_OF, Imports.INCLUDED)
                .filter(ax -> ax.getSubProperty() == prop)
                .map(ax -> new AnnotationPropertyNode(ax.getSuperProperty()))
                .collect(Collectors.toSet());

        if (parents.isEmpty()) {
            return Collections.singleton(dummyRootNode);
        }
        return parents;
    }

    @Override
    protected Node<OWLAnnotationProperty> equivs(OWLAnnotationProperty prop) {
        // TODO find subs of each other?
        return new AnnotationPropertyNode(prop);
    }

    public Stream<Node<OWLAnnotationProperty>> getDescendants(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return ont.annotationPropertiesInSignature(Imports.INCLUDED)
                    .map(AnnotationPropertyNode::new);
        }
        Set<Node<OWLAnnotationProperty>> subs = subs(prop);
        Stream<Node<OWLAnnotationProperty>> recursive = subs.stream().flatMap(sub -> getDescendants(sub.getRepresentativeElement()));
        return Streams.concat(
                subs.stream(),
                recursive);
    }

    class AnnotationPropertyNode extends DefaultNode<OWLAnnotationProperty> {

        public AnnotationPropertyNode(OWLAnnotationProperty entity) {
            super(entity);
        }

        @Override
        protected Optional<OWLAnnotationProperty> getTopEntity() {
            return Optional.of(dummyroot);
        }

        @Override
        protected Optional<OWLAnnotationProperty> getBottomEntity() {
            return Optional.empty();
        }
    }
}
