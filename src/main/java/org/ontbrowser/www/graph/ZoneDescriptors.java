package org.ontbrowser.www.graph;

import java.util.List;

public record ZoneDescriptors(
        List<String> topWithProperties,
        List<String> bottomWithProperties,
        List<String> leftWithProperties,
        List<String> rightWithProperties
) {}
