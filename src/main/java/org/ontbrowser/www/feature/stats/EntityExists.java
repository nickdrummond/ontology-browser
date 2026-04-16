package org.ontbrowser.www.feature.stats;

public record EntityExists(
        boolean classes,
        boolean individuals,
        boolean objectProperties,
        boolean dataProperties,
        boolean annotationProperties,
        boolean datatypes) {
}
