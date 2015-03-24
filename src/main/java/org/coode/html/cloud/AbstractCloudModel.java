package org.coode.html.cloud;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import java.util.*;


/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 15, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public abstract class AbstractCloudModel<O> implements CloudModel<O> {

    private Map<O, Integer> entityValueMap = new HashMap<O, Integer>();

    private int min;
    private int max;

    private List<ChangeListener> listeners = new ArrayList<ChangeListener>();

    public void reload() {
        min = Integer.MAX_VALUE;
        max = 0;

        entityValueMap.clear();

        for (O entity : getEntities()) {
            int value = calculateValue(entity);
            min = Math.min(min, value);
            max = Math.max(max, value);
            entityValueMap.put(entity, value);
        }

        ChangeEvent e = new ChangeEvent(this);
        for (ChangeListener l : listeners) {
            l.stateChanged(e);
        }
    }

    public abstract Set<O> getEntities();

    public final Set<O> getEntities(int threshold) {

        threshold = normalize(threshold);

        Set<O> result = new HashSet<O>();

        for (O entity : entityValueMap.keySet()) {
            if (entityValueMap.get(entity) >= threshold) {
                result.add(entity);
            }
        }
        return result;
    }

    public String getRendering(O entity) {
        return entity.toString();
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

    public void dispose() {
        listeners.clear();
    }

    public final void addChangeListener(ChangeListener l) {
        listeners.add(l);
    }

    public final void removeChangeListener(ChangeListener l) {
        listeners.remove(l);
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
