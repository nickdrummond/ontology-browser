package org.ontbrowser.www.model.characteristics;

import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;

public class DatatypeCharacteristicsBuilder extends CharacteristicsBuilder<OWLDatatype> {

    public static final String DEFINITION = "Definition";

    private static final List<String> order = List.of(
            ANNOTATIONS,
            DEFINITION,
            USAGE
    );

    public DatatypeCharacteristicsBuilder(
            OWLDatatype target,
            OWLOntology ont,
            Comparator<OWLObject> comparator,
            List<With> with,
            int pageSize) {
        super(target, ont, comparator, with, pageSize);
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
