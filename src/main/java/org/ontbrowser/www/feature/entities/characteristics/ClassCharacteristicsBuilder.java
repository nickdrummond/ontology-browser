package org.ontbrowser.www.feature.entities.characteristics;

import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;

import java.util.*;

public class ClassCharacteristicsBuilder extends CharacteristicsBuilder<OWLClass> {

    public static final String SUPERS = "Superclasses";
    public static final String EQUIV = "Equivalents";
    public static final String HASKEY = "Has Key";
    public static final String MEMBERS = "Members";
    public static final String DISJOINTS = "Disjoints";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            SUPERS,
            EQUIV,
            HASKEY,
            USAGE,
            MEMBERS,
            DISJOINTS
    );

    public ClassCharacteristicsBuilder(
            OWLClass target,
            OWLOntology ont,
            Comparator<OWLObject> comparator,
            List<With> with,
            int defaultPageSize) {
        super(target, ont, comparator, with, defaultPageSize);
    }

    @Override
    CharacteristicsBuilder<OWLClass>.InterestingFilter createFilter(OWLOntology ont, OWLClass target) {
        return new InterestingFilter(ont, target);
    }

    @Override
    List<String> getOrder() {
        return order;
    }

    class InterestingFilter extends CharacteristicsBuilder<OWLClass>.InterestingFilter {

        public InterestingFilter(OWLOntology ont, OWLClass target) {
            super(ont, target);
        }

        @Override
        public AxiomWithMetadata visit(OWLSubClassOfAxiom axiom) {
            return wrap(SUPERS, axiom,
                    axiom.getSubClass().equals(target),
                    axiom::getSuperClass);
        }

        @Override
        public AxiomWithMetadata visit(OWLEquivalentClassesAxiom axiom) {
            return wrap(EQUIV, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLHasKeyAxiom axiom) {
            return wrap(HASKEY, axiom,
                    axiom.getClassExpression().containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLClassAssertionAxiom axiom) {
            return wrap(MEMBERS, axiom,
                    axiom.getClassExpression().equals(target),
                    axiom::getIndividual);
        }

        @Override
        public AxiomWithMetadata visit(OWLDisjointClassesAxiom axiom) {
            return wrap(DISJOINTS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDisjointUnionAxiom axiom) {
            return wrap(DISJOINTS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }
    }
}
