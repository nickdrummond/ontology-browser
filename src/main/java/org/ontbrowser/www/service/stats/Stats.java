package org.ontbrowser.www.service.stats;

public interface Stats<T> {

    public int getStats(T target);

    public String getName();
}
