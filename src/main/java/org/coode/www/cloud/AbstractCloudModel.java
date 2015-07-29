package org.coode.www.cloud;

import com.google.common.collect.Sets;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public abstract class AbstractCloudModel<O> implements CloudModel<O> {

    private Map<O, Integer> entityValueMap = new HashMap<O, Integer>();

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
        return new Comparator<O>() {
            public int compare(O entity, O entity1) {
                // we want to reverse the score comparison, to show biggest first
                return entityValueMap.get(entity1).compareTo(entityValueMap.get(entity));
            }
        };
    }
}
