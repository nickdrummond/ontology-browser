package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.paging.With;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;

public class ObjectPropertyCharacteristicsBuilder extends CharacteristicsBuilder<OWLObjectProperty> {

    public static final String SUPERS = "Super properties";
    public static final String EQUIV = "Equivalents";
    public static final String DOMAIN = "Domain";
    public static final String RANGE = "Range";
    public static final String INVERSES = "Inverses";
    public static final String PROPERTY_CHAIN = "Property Chains";
    public static final String CHARACTERISTICS = "Characteristics";
    public static final String DISJOINTS = "Disjoints";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            SUPERS,
            EQUIV,
            DOMAIN,
            RANGE,
            INVERSES,
            PROPERTY_CHAIN,
            CHARACTERISTICS,
            USAGE,
            DISJOINTS
    );

    public ObjectPropertyCharacteristicsBuilder(
            OWLObjectProperty target,
            OWLOntology ont,
            Comparator<OWLObject> comparator,
            List<With> with,
            int pageSize) {
        super(target, ont, comparator, with, pageSize);
    }

    @Override
    CharacteristicsBuilder<OWLObjectProperty>.InterestingFilter createFilter(OWLOntology ont, OWLObjectProperty target) {
        return new InterestingFilter(ont, target);
    }

    @Override
    List<String> getOrder() {
        return order;
    }

    class InterestingFilter extends CharacteristicsBuilder<OWLObjectProperty>.InterestingFilter {

        public InterestingFilter(OWLOntology ont, OWLObjectProperty target) {
            super(ont, target);
        }

        @Override
        public AxiomWithMetadata visit(OWLSubObjectPropertyOfAxiom axiom) {
            return wrap(SUPERS, axiom,
                    axiom.getSubProperty().equals(target),
                    axiom::getSuperProperty);
        }

        @Override
        public AxiomWithMetadata visit(OWLEquivalentObjectPropertiesAxiom axiom) {
            return wrap(EQUIV, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLObjectPropertyDomainAxiom axiom) {
            return wrap(DOMAIN, axiom,
                    axiom.getProperty().equals(target),
                    axiom::getDomain);
        }

        @Override
        public AxiomWithMetadata visit(OWLObjectPropertyRangeAxiom axiom) {
            return wrap(RANGE, axiom,
                    axiom.getProperty().equals(target),
                    axiom::getRange);
        }

        @Override
        public AxiomWithMetadata visit(OWLInverseObjectPropertiesAxiom axiom) {
            return wrap(INVERSES, axiom,
                    axiom.getFirstProperty().equals(target),
                    axiom::getSecondProperty);
        }

        @Override
        public AxiomWithMetadata visit(OWLSubPropertyChainOfAxiom axiom) {
            return wrap(PROPERTY_CHAIN, axiom,
                    axiom.getSuperProperty().equals(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLFunctionalObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLInverseFunctionalObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLTransitiveObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLSymmetricObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLAsymmetricObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLReflexiveObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLIrreflexiveObjectPropertyAxiom axiom) {
            return characteristic(axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDisjointObjectPropertiesAxiom axiom) {
            return wrap(DISJOINTS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        private AxiomWithMetadata characteristic(OWLObjectPropertyCharacteristicAxiom axiom) {
            return wrap(CHARACTERISTICS, axiom,
                    axiom.getProperty().equals(target),
                    () -> axiom);
        }
    }
}
