package org.coode.www.model.timeline;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.coode.www.model.timeline.EventUtils.REMOVE_ME;
import static org.coode.www.model.timeline.EventUtils.buildConverging;
import static org.junit.Assert.assertEquals;

public class EventUtilsTest {


    @Test
    public void diverging() {
        TConn a1 = new TConn("A1", REMOVE_ME);
        TConn a2 = new TConn("A2", REMOVE_ME);
        TConn b1 = new TConn("B1", REMOVE_ME);
        TConn b2 = new TConn("B2", REMOVE_ME);
        TConn c1 = new TConn("C1", REMOVE_ME);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2),
                List.of(b1, b2),
                List.of(c1)
        );
        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(REMOVE_ME, List.of(a1, a2), true, false),
                new Timeline(REMOVE_ME, List.of(b1, b2), true, false),
                new Timeline(REMOVE_ME, List.of(c1), true, false)
        ), REMOVE_ME));

        assertEquals(expected, chain);
    }

    @Test
    public void convergingLastElement() {
        TConn a1 = new TConn("A1", REMOVE_ME);
        TConn a2 = new TConn("A2", REMOVE_ME);
        TConn b1 = new TConn("B1", REMOVE_ME);
        TConn b2 = new TConn("B2", REMOVE_ME);
        TConn c1 = new TConn("C1", REMOVE_ME);
        TConn common1 = new TConn("COMMON1", REMOVE_ME);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1),
                List.of(b1, b2, common1),
                List.of(c1, common1)
        );
        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(REMOVE_ME, List.of(a1, a2), true, true),
                new Timeline(REMOVE_ME, List.of(b1, b2), true, true),
                new Timeline(REMOVE_ME, List.of(c1), true, true)
        ), REMOVE_ME), common1);

        assertEquals(expected, chain);
    }

    @Test
    public void convergingLast2Element() {
        TConn a1 = new TConn("A1", REMOVE_ME);
        TConn a2 = new TConn("A2", REMOVE_ME);
        TConn b1 = new TConn("B1", REMOVE_ME);
        TConn b2 = new TConn("B2", REMOVE_ME);
        TConn c1 = new TConn("C1", REMOVE_ME);
        TConn common1 = new TConn("COMMON1", REMOVE_ME);
        TConn common2 = new TConn("COMMON2", REMOVE_ME);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2),
                List.of(c1, common1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(REMOVE_ME, List.of(a1, a2), true, true),
                new Timeline(REMOVE_ME, List.of(b1, b2), true, true),
                new Timeline(REMOVE_ME, List.of(c1), true, true)
        ), REMOVE_ME), common1, common2);

        assertEquals(expected, chain);
    }

    @Test
    public void converging2Paths() {
        TConn a1 = new TConn("A1", REMOVE_ME);
        TConn a2 = new TConn("A2", REMOVE_ME);
        TConn b1 = new TConn("B1", REMOVE_ME);
        TConn b2 = new TConn("B2", REMOVE_ME);
        TConn c1 = new TConn("C1", REMOVE_ME);
        TConn common1 = new TConn("COMMON1", REMOVE_ME);
        TConn common2 = new TConn("COMMON2", REMOVE_ME);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(REMOVE_ME, List.of(a1, a2), true, true),
                new Timeline(REMOVE_ME, List.of(b1, b2), true, true)
        ), REMOVE_ME), common1, common2);

        assertEquals(expected, chain);
    }

    @Test
    public void converging2PathsConvergeEarlier() {
        TConn a1 = new TConn("A1", REMOVE_ME);
        TConn a2 = new TConn("A2", REMOVE_ME);
        TConn b1 = new TConn("B1", REMOVE_ME);
        TConn b2 = new TConn("B2", REMOVE_ME);
        TConn c1 = new TConn("C1", REMOVE_ME);
        TConn common1 = new TConn("COMMON1", REMOVE_ME);
        TConn common2 = new TConn("COMMON2", REMOVE_ME);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2),
                List.of(c1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        Timeline aTimeline = new Timeline(REMOVE_ME, List.of(a1, a2), true, true);
        Timeline bTimeline = new Timeline(REMOVE_ME, List.of(b1, b2), true, true);
        Timeline cTimeline = new Timeline(REMOVE_ME, List.of(c1), true, true);

        Timeline p2 = new Timeline(REMOVE_ME, List.of(new TConn(List.of(
                aTimeline,
                bTimeline
        ), REMOVE_ME), common1), true, true);

        TConn p1 = new TConn(List.of(p2, cTimeline), REMOVE_ME);

        List<TConn> expected = List.of(p1, common2);

        assertEquals(2, chain.size());
        assertEquals(p2, ((TParallel)chain.get(0).node()).timelines().get(0));
        assertEquals(cTimeline, ((TParallel)chain.get(0).node()).timelines().get(1)); // TODO should be cTimeline - but same as p2
        assertEquals(p1, chain.get(0));
        assertEquals(expected, chain);
    }
}