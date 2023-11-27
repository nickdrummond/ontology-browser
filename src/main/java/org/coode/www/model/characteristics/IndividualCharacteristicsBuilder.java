package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.characteristics.CharacteristicsBuilder;
import org.semanticweb.owlapi.model.*;

import java.util.*;

import static java.util.stream.Collectors.groupingBy;

public class IndividualCharacteristicsBuilder extends CharacteristicsBuilder<OWLNamedIndividual> {
    public static final String TYPES = "Types";
    public static final String OBJECT_PROPERTY_ASSERTIONS = "Object Property Assertions";
    public static final String OBJECT_PROPERTY_ASSERTIONS_INVERSE = "Object Property Assertions (inverse)";
    public static final String NEGATIVE_OBJECT_PROPERTY_ASSERTIONS = "Negative Object Property Assertions";
    public static final String DATA_PROPERTY_ASSERTIONS = "Data Property Assertions";
    public static final String NEGATIVE_DATA_PROPERTY_ASSERTIONS = "Negative Data Property Assertions";
    public static final String SAME_AS = "Same As";
    public static final String DIFFERENT = "Different";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            TYPES,
            OBJECT_PROPERTY_ASSERTIONS,
            DATA_PROPERTY_ASSERTIONS,
            NEGATIVE_OBJECT_PROPERTY_ASSERTIONS,
            NEGATIVE_DATA_PROPERTY_ASSERTIONS,
            OBJECT_PROPERTY_ASSERTIONS_INVERSE,
            USAGE,
            SAME_AS,
            DIFFERENT
    );

    public IndividualCharacteristicsBuilder(OWLNamedIndividual target,
                                            Set<OWLOntology> activeOntologies,
                                            Comparator<OWLObject> comparator) {
        super(target, activeOntologies, comparator);
    }

    @Override
    CharacteristicsBuilder<OWLNamedIndividual>.InterestingFilter createFilter(OWLOntology ont, OWLNamedIndividual target) {
        return new InterestingFilter(ont, target);
    }

    @Override
    List<String> getOrder() {
        return order;
    }

    class InterestingFilter extends CharacteristicsBuilder<OWLNamedIndividual>.InterestingFilter {

        public InterestingFilter(OWLOntology ont, OWLNamedIndividual target) {
            super(ont, target);
        }

        @Override
        public AxiomWithMetadata visit(OWLClassAssertionAxiom axiom) {
            return doIt(TYPES, axiom,
                    axiom.getIndividual().equals(target),
                    axiom::getClassExpression);
        }

        @Override
        public AxiomWithMetadata visit(OWLObjectPropertyAssertionAxiom axiom) {
            if (axiom.getSubject().equals(target)) {
                return new AxiomWithMetadata(OBJECT_PROPERTY_ASSERTIONS, axiom, axiom, ont);
            }
            else if (axiom.getObject().equals(target)) {
                return new AxiomWithMetadata(OBJECT_PROPERTY_ASSERTIONS_INVERSE, axiom, axiom, ont);
            }
            return new AxiomWithMetadata(USAGE, axiom, axiom, ont);
        }

        @Override
        public AxiomWithMetadata visit(OWLNegativeObjectPropertyAssertionAxiom axiom) {
            return doIt(NEGATIVE_OBJECT_PROPERTY_ASSERTIONS, axiom,
                    axiom.getSubject().equals(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDataPropertyAssertionAxiom axiom) {
            return doIt(DATA_PROPERTY_ASSERTIONS, axiom,
                    axiom.getSubject().equals(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLNegativeDataPropertyAssertionAxiom axiom) {
            return doIt(NEGATIVE_DATA_PROPERTY_ASSERTIONS, axiom,
                    axiom.getSubject().equals(target),
                    axiom::getObject);
        }

        @Override
        public AxiomWithMetadata visit(OWLSameIndividualAxiom axiom) {
            return doIt(SAME_AS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDifferentIndividualsAxiom axiom) {
            return doIt(DIFFERENT, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }
    }
}
