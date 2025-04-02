package org.ontbrowser.www.feature.stats;

import java.util.Set;

public record OntologyStats(
        EntityCounts entityCounts,
        AxiomCounts axiomCounts,
        AxiomTypeCounts axiomTypeCounts,
        Set<Coords<Long>> classChildCountDistribution
) {
}
