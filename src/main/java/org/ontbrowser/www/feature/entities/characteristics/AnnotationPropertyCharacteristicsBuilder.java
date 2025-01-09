package org.ontbrowser.www.feature.entities.characteristics;

import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;

public class AnnotationPropertyCharacteristicsBuilder extends CharacteristicsBuilder<OWLAnnotationProperty> {

    public static final String SUPERS = "Super properties";
    public static final String DOMAIN = "Domain";
    public static final String RANGE = "Range";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            SUPERS,
            DOMAIN,
            RANGE,
            USAGE
    );

    public AnnotationPropertyCharacteristicsBuilder(
            OWLAnnotationProperty target,
            OWLOntology ont,
            Comparator<OWLObject> comparator,
            List<With> with,
            int pageSize) {
        super(target, ont, comparator, with, pageSize);
    }

    @Override
    CharacteristicsBuilder<OWLAnnotationProperty>.InterestingFilter createFilter(OWLOntology ont, OWLAnnotationProperty target) {
        return new InterestingFilter(ont, target);
    }

    @Override
    List<String> getOrder() {
        return order;
    }

    class InterestingFilter extends CharacteristicsBuilder<OWLAnnotationProperty>.InterestingFilter {

        public InterestingFilter(OWLOntology ont, OWLAnnotationProperty target) {
            super(ont, target);
        }

        @Override
        public AxiomWithMetadata visit(OWLSubAnnotationPropertyOfAxiom axiom) {
            return wrap(SUPERS, axiom,
                    axiom.getSubProperty().equals(target),
                    axiom::getSuperProperty);
        }

        @Override
        public AxiomWithMetadata visit(OWLAnnotationPropertyDomainAxiom axiom) {
            return wrap(DOMAIN, axiom,
                    axiom.getProperty().equals(target),
                    axiom::getDomain);
        }

        @Override
        public AxiomWithMetadata visit(OWLAnnotationPropertyRangeAxiom axiom) {
            return wrap(RANGE, axiom,
                    axiom.getProperty().equals(target),
                    axiom::getRange);
        }
    }
}
