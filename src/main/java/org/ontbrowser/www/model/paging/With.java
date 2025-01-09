package org.ontbrowser.www.model.paging;

import java.util.List;
import java.util.Objects;

public record With(String characteristicName, int start, int pageSize) {
    public static With getOrDefault(String name, List<With> withs) {
        return withs.stream()
                .filter(w -> Objects.equals(w.characteristicName(), name)).findFirst()
                .orElse(new With(name, 1, 30)); // default
    }
}