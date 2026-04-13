package org.ontbrowser.www.feature.cloud.model;

import org.semanticweb.owlapi.model.OWLEntity;

import java.util.Comparator;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * A CloudModel whose entity→usage values are supplied up-front (e.g. from a single
 * DB query) rather than computed one-by-one via the OWL API.
 * Implements {@link CloudModel} directly to avoid the double-map overhead that would
 * arise from extending {@link AbstractCloudModel}.
 */
class PreloadedCloudModel<T extends OWLEntity> implements CloudModel<T> {

    private final Map<T, Integer> valueMap;
    private int min;
    private int max;

    PreloadedCloudModel(Map<T, Integer> valueMap) {
        this.valueMap = valueMap;
    }

    @Override
    public void load() {
        min = Integer.MAX_VALUE;
        max = 0;
        for (int v : valueMap.values()) {
            min = Math.min(min, v);
            max = Math.max(max, v);
        }
    }

    @Override
    public int getValue(T entity) {
        return valueMap.get(entity);
    }

    @Override
    public int getMin() { return min; }

    @Override
    public int getMax() { return max; }

    @Override
    public int getRange() { return max - min; }

    @Override
    public Set<T> getEntities() {
        return valueMap.keySet();
    }

    @Override
    public Set<T> getEntities(int threshold) {
        int normalised = min + ((max - min) * threshold) / 100;
        return valueMap.entrySet().stream()
                .filter(e -> e.getValue() >= normalised)
                .map(Map.Entry::getKey)
                .collect(Collectors.toSet());
    }

    @Override
    public Comparator<T> getComparator() {
        return Comparator.comparing(valueMap::get);
    }
}
