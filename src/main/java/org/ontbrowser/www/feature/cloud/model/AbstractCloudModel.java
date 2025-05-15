package org.ontbrowser.www.feature.cloud.model;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import java.util.Comparator;
import java.util.Map;
import java.util.Set;

abstract class AbstractCloudModel<O> implements CloudModel<O> {

    private final Map<O, Integer> entityValueMap = Maps.newHashMap();

    private int min;
    private int max;

    public void load() {
        min = Integer.MAX_VALUE;
        max = 0;

        entityValueMap.clear();

        for (O entity : getEntities()) {
            int value = calculateValue(entity);
            min = Math.min(min, value);
            max = Math.max(max, value);
            entityValueMap.put(entity, value);
        }
    }

    public final Set<O> getEntities(final int threshold) {

        var normalisedThreshold = normalize(threshold);

        Set<O> result = Sets.newHashSet();

        for (O entity : entityValueMap.keySet()) {
            if (entityValueMap.get(entity) >= normalisedThreshold) {
                result.add(entity);
            }
        }
        return result;
    }

    protected abstract int calculateValue(O entity);

    public final int getValue(O entity) {
        return entityValueMap.get(entity);
    }

    public final int getMin() {
        return min;
    }

    public final int getMax() {
        return max;
    }

    public final int getRange() {
        return max - min;
    }

    private int normalize(int threshold) {
        int range = max - min;
        threshold = min + (range * threshold) / 100;
        return threshold;
    }

    public Comparator<O> getComparator() {
        return Comparator.comparing(entityValueMap::get);
    }
}
