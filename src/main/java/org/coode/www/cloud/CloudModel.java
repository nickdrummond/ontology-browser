package org.coode.www.cloud;

import java.net.URL;
import java.util.Comparator;
import java.util.Set;

public interface CloudModel<O> {

    int getValue(O entity);

    int getMin();

    int getMax();

    int getRange();

    Set<O> getEntities();

    Set<O> getEntities(int threshold);

    String getRendering(O entity);

    Comparator<O> getComparator();

    URL getURL(O entity);

    void reload();

    String getTitle();
}
