package org.ontbrowser.www.feature.graph;

import java.util.List;

public record ZoneDescriptors(
        List<String> top,
        List<String> bottom,
        List<String> left,
        List<String> right
) {}
