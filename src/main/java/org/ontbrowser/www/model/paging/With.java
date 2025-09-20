package org.ontbrowser.www.model.paging;

import java.util.List;
import java.util.Objects;

public record With(String characteristicName, int start, int pageSize) {

    @Override
    public String toString() {
        return characteristicName + " " + start + " " + pageSize;
    }

    public static With getOrDefault(String name, List<With> withs) {
        return withs.stream()
                .filter(w -> Objects.equals(w.characteristicName(), name)).findFirst()
                .orElse(new With(name, 1, 30)); // default
    }

    public static With valueOf(String text) {
        String[] parts = text.split(" ");
        int start = Math.max(1, Integer.parseInt(parts[1]));
        int pageSize = Math.max(1, Integer.parseInt(parts[2]));
        return new With(parts[0], start, pageSize);
    }
}