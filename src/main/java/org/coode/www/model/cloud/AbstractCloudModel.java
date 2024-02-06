package org.coode.www.model.cloud;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;

import java.util.Comparator;
import java.util.Map;
import java.util.Set;

public abstract class AbstractCloudModel<O> implements CloudModel<O> {

    private final Map<O, Integer> entityValueMap = Maps.newHashMap();

    private int min;
    private int max;

    protected void reload() {
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

    public abstract Set<O> getEntities();

    public final Set<O> getEntities(int threshold) {

        threshold = normalize(threshold);

        Set<O> result = Sets.newHashSet();

        for (O entity : entityValueMap.keySet()) {
            if (entityValueMap.get(entity) >= threshold) {
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
