package org.ontbrowser.www.util;


import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;


public class PairwiseOrderingTest {

    @SafeVarargs
    private final <T> List<T> l(T... elements) {
        return Arrays.asList(elements);
    }

    @Test
    public void testJoinTwoPairsOnOverlap() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('A', 'B'));
        ordering.add(l('B', 'C'));

        assertEquals(l(l('A', 'B', 'C')), ordering.getResult());
    }

    @Test
    public void testJoinOutOfSequence() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('B', 'C'));
        ordering.add(l('A', 'B'));

        assertEquals(l(l('A', 'B', 'C')), ordering.getResult());
    }

    @Test
    public void testJoinPrevious() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('A', 'B'));
        ordering.add(l('C', 'D'));
        ordering.add(l('B', 'C'));

        assertEquals(l(l('A', 'B', 'C', 'D')), ordering.getResult());
    }

    @Test
    public void testDiscontiguous() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('A', 'B'));
        ordering.add(l('S', 'T'));
        ordering.add(l('B', 'C'));
        ordering.add(l('R', 'S'));

        assertEquals(l(
                l('A', 'B', 'C'),
                l('R', 'S', 'T')), ordering.getResult());
    }

    @Disabled
    @Test
    public void testNaturalOrdering() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('S', 'T'));
        ordering.add(l('B', 'C'));
        ordering.add(l('R', 'S'));
        ordering.add(l('A', 'B'));

        assertEquals(l(
                l('A', 'B', 'C'),
                l('R', 'S', 'T')), ordering.getResult());
    }

    @Test
    public void testLongerLists() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('A', 'B'));
        ordering.add(l('S', 'T', 'U', 'V'));
        ordering.add(l('B', 'C', 'D', 'E', 'F'));
        ordering.add(l('Q', 'R', 'S'));

        assertEquals(l(
                l('A', 'B', 'C', 'D', 'E', 'F'),
                l('Q', 'R', 'S', 'T', 'U', 'V')), ordering.getResult());
    }

    @Test
    public void testFlattened() {
        PairwiseOrdering<Character> ordering = new PairwiseOrdering<>();
        ordering.add(l('A', 'B'));
        ordering.add(l('S', 'T', 'U', 'V'));
        ordering.add(l('B', 'C', 'D', 'E', 'F'));
        ordering.add(l('Q', 'R', 'S'));

        assertEquals(l('A', 'B', 'C', 'D', 'E', 'F', 'Q', 'R', 'S', 'T', 'U', 'V'), ordering.flattened());
    }
}
