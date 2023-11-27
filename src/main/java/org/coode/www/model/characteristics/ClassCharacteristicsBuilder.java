package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.characteristics.CharacteristicsBuilder;
import org.semanticweb.owlapi.model.*;

import java.util.*;

public class ClassCharacteristicsBuilder extends CharacteristicsBuilder<OWLClass> {

    public static final String SUPERS = "Superclasses";
    public static final String EQUIV = "Equivalents";
    public static final String MEMBERS = "Members";
    public static final String DISJOINTS = "Disjoints";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            SUPERS,
            EQUIV,
            USAGE,
            MEMBERS,
            DISJOINTS
    );

    public ClassCharacteristicsBuilder(OWLClass target,
                                       Set<OWLOntology> activeOntologies,
                                       Comparator<OWLObject> comparator) {
        super(target, activeOntologies, comparator);
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
            return doIt(SUPERS, axiom,
                    axiom.getSubClass().equals(target),
                    axiom::getSuperClass);
        }

        @Override
        public AxiomWithMetadata visit(OWLEquivalentClassesAxiom axiom) {
            return doIt(EQUIV, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLClassAssertionAxiom axiom) {
            return doIt(MEMBERS, axiom,
                    axiom.getClassExpression().equals(target),
                    axiom::getIndividual);
        }

        @Override
        public AxiomWithMetadata visit(OWLDisjointClassesAxiom axiom) {
            return doIt(DISJOINTS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDisjointUnionAxiom axiom) {
            return doIt(DISJOINTS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }
    }
}
