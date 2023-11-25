package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.impl.DefaultNode;
import org.semanticweb.owlapi.reasoner.impl.OWLObjectPropertyNode;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

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
            return ont.getAnnotationPropertiesInSignature(Imports.INCLUDED).stream()
                    .map(AnnotationPropertyNode::new).collect(Collectors.toSet());
        }
        return Collections.emptySet();
    }

    @Override
    protected Set<Node<OWLAnnotationProperty>> ancestors(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return Collections.emptySet();
        }
        return Set.of(dummyRootNode);
    }

    @Override
    protected Node<OWLAnnotationProperty> equivs(OWLAnnotationProperty prop) {
        return new AnnotationPropertyNode(prop);
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
