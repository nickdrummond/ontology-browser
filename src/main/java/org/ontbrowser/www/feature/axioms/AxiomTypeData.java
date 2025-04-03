package org.ontbrowser.www.feature.axioms;

import org.semanticweb.owlapi.model.AxiomType;

public record AxiomTypeData(
        AxiomType<?> type,
        String name,
        String category
) {
}