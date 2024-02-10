package org.coode.www.model.characteristics;

import org.coode.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;
import java.util.Set;

public class DatatypeCharacteristicsBuilder extends CharacteristicsBuilder<OWLDatatype> {

    public static final String DEFINITION = "Definition";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            DEFINITION,
            USAGE
    );

    public DatatypeCharacteristicsBuilder(OWLDatatype target,
                                          OWLOntology ont,
                                          Comparator<OWLObject> comparator) {
        super(target, ont, comparator);
    }

    @Override
    CharacteristicsBuilder<OWLDatatype>.InterestingFilter createFilter(OWLOntology ont, OWLDatatype target) {
        return new InterestingFilter(ont, target);
    }

    @Override
    List<String> getOrder() {
        return order;
    }

    class InterestingFilter extends CharacteristicsBuilder<OWLDatatype>.InterestingFilter {

        public InterestingFilter(OWLOntology ont, OWLDatatype target) {
            super(ont, target);
        }

        @Override
        public AxiomWithMetadata visit(OWLDatatypeDefinitionAxiom axiom) {
            return wrap(DEFINITION, axiom,
                    axiom.getDatatype().equals(target),
                    axiom::getDataRange);
        }
    }
}
