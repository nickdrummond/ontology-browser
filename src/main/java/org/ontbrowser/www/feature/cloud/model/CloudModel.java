package org.ontbrowser.www.feature.cloud.model;

import java.util.Comparator;
import java.util.Set;

public interface CloudModel<O> {

    void load();

    int getValue(O entity);

    int getMin();

    int getMax();

    int getRange();

    Set<O> getEntities();

    Set<O> getEntities(int threshold);

    Comparator<O> getComparator();
}
