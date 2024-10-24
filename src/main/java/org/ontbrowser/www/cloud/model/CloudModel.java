package org.ontbrowser.www.cloud.model;

import java.util.Comparator;
import java.util.Set;

public interface CloudModel<O> {

    int getValue(O entity);

    int getMin();

    int getMax();

    int getRange();

    Set<O> getEntities();

    Set<O> getEntities(int threshold);

    Comparator<O> getComparator();
}
