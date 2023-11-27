package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class DataPropertyCharacteristicsBuilder extends CharacteristicsBuilder<OWLDataProperty> {

    public static final String SUPERS = "Super properties";
    public static final String EQUIV = "Equivalents";
    public static final String DOMAIN = "Domain";
    public static final String RANGE = "Range";
    public static final String CHARACTERISTICS = "Characteristics";
    public static final String DISJOINTS = "Disjoints";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            SUPERS,
            EQUIV,
            DOMAIN,
            RANGE,
            CHARACTERISTICS,
            USAGE,
            DISJOINTS
    );

    public DataPropertyCharacteristicsBuilder(OWLDataProperty target,
                                              Set<OWLOntology> activeOntologies,
                                              Comparator<OWLObject> comparator) {
        super(target, activeOntologies, comparator);
    }

    @Override
    CharacteristicsBuilder<OWLDataProperty>.InterestingFilter createFilter(OWLOntology ont, OWLDataProperty target) {
        return new InterestingFilter(ont, target);
    }

    @Override
    List<String> getOrder() {
        return order;
    }

    class InterestingFilter extends CharacteristicsBuilder<OWLDataProperty>.InterestingFilter {

        public InterestingFilter(OWLOntology ont, OWLDataProperty target) {
            super(ont, target);
        }

        @Override
        public AxiomWithMetadata visit(OWLSubDataPropertyOfAxiom axiom) {
            return doIt(SUPERS, axiom,
                    axiom.getSubProperty().equals(target),
                    axiom::getSuperProperty);
        }

        @Override
        public AxiomWithMetadata visit(OWLEquivalentDataPropertiesAxiom axiom) {
            return doIt(EQUIV, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDataPropertyDomainAxiom axiom) {
            return doIt(DOMAIN, axiom,
                    axiom.getProperty().equals(target),
                    axiom::getDomain);
        }

        @Override
        public AxiomWithMetadata visit(OWLDataPropertyRangeAxiom axiom) {
            return doIt(RANGE, axiom,
                    axiom.getProperty().equals(target),
                    axiom::getRange);
        }

        @Override
        public AxiomWithMetadata visit(OWLFunctionalDataPropertyAxiom axiom) {
            return doIt(CHARACTERISTICS, axiom,
                    axiom.getProperty().equals(target),
                    () -> axiom);
        }

        @Override
        public AxiomWithMetadata visit(OWLDisjointDataPropertiesAxiom axiom) {
            return doIt(DISJOINTS, axiom,
                    axiom.containsEntityInSignature(target),
                    () -> axiom);
        }
    }
}
