package org.ontbrowser.www.feature.graph;

import java.util.List;

public record ZoneDescriptors(
        List<String> topWithProperties,
        List<String> bottomWithProperties,
        List<String> leftWithProperties,
        List<String> rightWithProperties
) {}
