package org.ontbrowser.www.feature.stats;

public interface Stats<T> {

    public int getStats(T target);

    public String getName();
}
