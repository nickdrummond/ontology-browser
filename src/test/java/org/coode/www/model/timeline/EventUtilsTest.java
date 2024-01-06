package org.coode.www.model.timeline;

import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.coode.www.model.timeline.EventUtils.AFTER;
import static org.coode.www.model.timeline.EventUtils.buildConverging;
import static org.junit.Assert.assertEquals;

public class EventUtilsTest {


    @Test
    public void diverging() {
        TConn a1 = new TConn("A1", AFTER);
        TConn a2 = new TConn("A2", AFTER);
        TConn b1 = new TConn("B1", AFTER);
        TConn b2 = new TConn("B2", AFTER);
        TConn c1 = new TConn("C1", AFTER);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2),
                List.of(b1, b2),
                List.of(c1)
        );
        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(AFTER, List.of(a1, a2), true, false),
                new Timeline(AFTER, List.of(b1, b2), true, false),
                new Timeline(AFTER, List.of(c1), true, false)
        ), AFTER));

        assertEquals(expected, chain);
    }

    @Test
    public void convergingLastElement() {
        TConn a1 = new TConn("A1", AFTER);
        TConn a2 = new TConn("A2", AFTER);
        TConn b1 = new TConn("B1", AFTER);
        TConn b2 = new TConn("B2", AFTER);
        TConn c1 = new TConn("C1", AFTER);
        TConn common1 = new TConn("COMMON1", AFTER);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1),
                List.of(b1, b2, common1),
                List.of(c1, common1)
        );
        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(AFTER, List.of(a1, a2), true, true),
                new Timeline(AFTER, List.of(b1, b2), true, true),
                new Timeline(AFTER, List.of(c1), true, true)
        ), AFTER), common1);

        assertEquals(expected, chain);
    }

    @Test
    public void convergingLast2Element() {
        TConn a1 = new TConn("A1", AFTER);
        TConn a2 = new TConn("A2", AFTER);
        TConn b1 = new TConn("B1", AFTER);
        TConn b2 = new TConn("B2", AFTER);
        TConn c1 = new TConn("C1", AFTER);
        TConn common1 = new TConn("COMMON1", AFTER);
        TConn common2 = new TConn("COMMON2", AFTER);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2),
                List.of(c1, common1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(AFTER, List.of(a1, a2), true, true),
                new Timeline(AFTER, List.of(b1, b2), true, true),
                new Timeline(AFTER, List.of(c1), true, true)
        ), AFTER), common1, common2);

        assertEquals(expected, chain);
    }

    @Test
    public void converging2Paths() {
        TConn a1 = new TConn("A1", AFTER);
        TConn a2 = new TConn("A2", AFTER);
        TConn b1 = new TConn("B1", AFTER);
        TConn b2 = new TConn("B2", AFTER);
        TConn c1 = new TConn("C1", AFTER);
        TConn common1 = new TConn("COMMON1", AFTER);
        TConn common2 = new TConn("COMMON2", AFTER);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(List.of(
                new Timeline(AFTER, List.of(a1, a2), true, true),
                new Timeline(AFTER, List.of(b1, b2), true, true)
        ), AFTER), common1, common2);

        assertEquals(expected, chain);
    }

    @Test
    public void converging2PathsConvergeEarlier() {
        TConn a1 = new TConn("A1", AFTER);
        TConn a2 = new TConn("A2", AFTER);
        TConn b1 = new TConn("B1", AFTER);
        TConn b2 = new TConn("B2", AFTER);
        TConn c1 = new TConn("C1", AFTER);
        TConn common1 = new TConn("COMMON1", AFTER);
        TConn common2 = new TConn("COMMON2", AFTER);
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2),
                List.of(c1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        Timeline aTimeline = new Timeline(AFTER, List.of(a1, a2), true, true);
        Timeline bTimeline = new Timeline(AFTER, List.of(b1, b2), true, true);
        Timeline cTimeline = new Timeline(AFTER, List.of(c1), true, true);

        Timeline p2 = new Timeline(AFTER, List.of(new TConn(List.of(
                aTimeline,
                bTimeline
        ), AFTER), common1), true, true);

        TConn p1 = new TConn(List.of(p2, cTimeline), AFTER);

        List<TConn> expected = List.of(p1, common2);

        assertEquals(2, chain.size());
        assertEquals(p2, ((TParallel)chain.get(0).node()).timelines().get(0));
        assertEquals(cTimeline, ((TParallel)chain.get(0).node()).timelines().get(1)); // TODO should be cTimeline - but same as p2
        assertEquals(p1, chain.get(0));
        assertEquals(expected, chain);
    }
}