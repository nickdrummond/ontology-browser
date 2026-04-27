package org.ontbrowser.www.feature.stats;

public record EntityCounts(
        long classes,
        long individuals,
        long objectProperties,
        long dataProperties,
        long annotationProperties,
        long datatypes) {
}
