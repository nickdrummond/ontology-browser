package org.ontbrowser.www.feature.stats;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StatsHelper {

    public static <I, O> Set<Coords<O>> getBarData(Stream<I> input, Function<I, O> groupBy) {
        Map<O, Long> collect = input
                .collect(Collectors.groupingBy(groupBy, Collectors.counting()));
        return collect
                .entrySet().stream()
                .map(entry -> new Coords<>(entry.getKey(), entry.getValue()))
                .collect(Collectors.toSet());
    }
}
