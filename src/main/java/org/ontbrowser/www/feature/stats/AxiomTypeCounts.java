package org.ontbrowser.www.feature.stats;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.semanticweb.owlapi.model.AxiomType;

import java.util.HashMap;
import java.util.Map;

public class AxiomTypeCounts {

    private final Map<AxiomType<?>, Integer> counts = new HashMap<>();

    public Map<AxiomType<?>, Integer> getCounts() {
        return counts;
    }

    @JsonIgnore
    public void put(AxiomType<?> axiomType, int axiomCount) {
        counts.put(axiomType, axiomCount);
    }
}
