package org.ontbrowser.www.service.stats;

import java.util.Arrays;

public class StatsMemo {
    private final String[] keys;

    public StatsMemo(String... keys) {
        this.keys = keys;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        StatsMemo statsMemo = (StatsMemo) o;
        return Arrays.equals(keys, statsMemo.keys);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(keys);
    }

    @Override
    public String toString() {
        return "StatsMemo{" +
                "keys=" + Arrays.toString(keys) +
                '}';
    }
}
