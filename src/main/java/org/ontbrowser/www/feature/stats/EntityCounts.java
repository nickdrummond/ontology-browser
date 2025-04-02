package org.ontbrowser.www.feature.stats;

public record EntityCounts(
        int classes,
        int individuals,
        int objectProperties,
        int dataProperties,
        int annotationProperties,
        int datatypes) {
}
