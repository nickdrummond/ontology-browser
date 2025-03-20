package org.ontbrowser.www.feature.stats;

public record AxiomCounts(
        int logicalAxioms,
        int Declaration,
        int AnnotationAssertion
) {
}
