package org.ontbrowser.www.feature.stats;


import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.ontbrowser.www.feature.stats.StatsHelper.getBarData;


public class StatsHelperTest {

    @Test
    public void testGetBarData() {
        var strings = List.of("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten");
        var result = getBarData(strings.stream(), String::length);
        var expected = Set.of(
                new Coords<>(3, 4),
                new Coords<>(4, 3),
                new Coords<>(5, 3)
        );
        assertEquals(expected, result);
    }
}