package org.coode.www.service.hierarchy;

import org.coode.www.model.Tree;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.impl.DefaultNode;

import java.util.*;
import java.util.stream.Collectors;

// TODO hierarchy
public class OWLAnnotationPropertyHierarchyService extends AbstractOWLHierarchyService<OWLAnnotationProperty> {

    private final OWLOntology ont;
    private final OWLAnnotationProperty dummyroot;
    public final Set<OWLAnnotationProperty> dummyRootNode;

    public OWLAnnotationPropertyHierarchyService(
            final OWLOntology ont,
            final Comparator<? super Tree<OWLAnnotationProperty>> comparator) {
        super(comparator);
        this.ont = ont;
        dummyroot = ont.getOWLOntologyManager().getOWLDataFactory().getOWLAnnotationProperty("dummyroot");
        dummyRootNode = Set.of(dummyroot);
    }

    @Override
    protected boolean isBottomNode(Set<OWLAnnotationProperty> subNode) {
        return false;
    }

    @Override
    protected Set<OWLAnnotationProperty> topNode() {
        return dummyRootNode;
    }

    @Override
    protected Set<Set<OWLAnnotationProperty>> subs(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return ont.getAnnotationPropertiesInSignature(Imports.INCLUDED).stream()
                    .map(Set::of).collect(Collectors.toSet());
        }
        return Collections.emptySet();
    }

    @Override
    protected Set<Set<OWLAnnotationProperty>> ancestors(OWLAnnotationProperty prop) {
        if (prop == dummyroot) {
            return Collections.emptySet();
        }
        return Set.of(dummyRootNode);
    }

    @Override
    protected Set<OWLAnnotationProperty> equivs(OWLAnnotationProperty prop) {
        return Set.of(prop);
    }
}
