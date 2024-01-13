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
        TConn a1 = new TConn(REMOVE_ME, "A1");
        TConn a2 = new TConn(REMOVE_ME, "A2");
        TConn b1 = new TConn(REMOVE_ME, "B1");
        TConn b2 = new TConn(REMOVE_ME, "B2");
        TConn c1 = new TConn(REMOVE_ME, "C1");
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2),
                List.of(b1, b2),
                List.of(c1)
        );
        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(REMOVE_ME, List.of(
                new Timeline(List.of(a1, a2), REMOVE_ME, true, false),
                new Timeline(List.of(b1, b2), REMOVE_ME, true, false),
                new Timeline(List.of(c1), REMOVE_ME, true, false)
        )));

        assertEquals(expected, chain);
    }

    @Test
    public void convergingLastElement() {
        TConn a1 = new TConn(REMOVE_ME, "A1");
        TConn a2 = new TConn(REMOVE_ME, "A2");
        TConn b1 = new TConn(REMOVE_ME, "B1");
        TConn b2 = new TConn(REMOVE_ME, "B2");
        TConn c1 = new TConn(REMOVE_ME, "C1");
        TConn common1 = new TConn(REMOVE_ME, "COMMON1");
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1),
                List.of(b1, b2, common1),
                List.of(c1, common1)
        );
        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(REMOVE_ME, List.of(
                new Timeline(List.of(a1, a2), REMOVE_ME, true, true),
                new Timeline(List.of(b1, b2), REMOVE_ME, true, true),
                new Timeline(List.of(c1), REMOVE_ME, true, true)
        )), common1);

        assertEquals(expected, chain);
    }

    @Test
    public void convergingLast2Element() {
        TConn a1 = new TConn(REMOVE_ME, "A1");
        TConn a2 = new TConn(REMOVE_ME, "A2");
        TConn b1 = new TConn(REMOVE_ME, "B1");
        TConn b2 = new TConn(REMOVE_ME, "B2");
        TConn c1 = new TConn(REMOVE_ME, "C1");
        TConn common1 = new TConn(REMOVE_ME, "COMMON1");
        TConn common2 = new TConn(REMOVE_ME, "COMMON2");
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2),
                List.of(c1, common1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(REMOVE_ME, List.of(
                new Timeline(List.of(a1, a2), REMOVE_ME, true, true),
                new Timeline(List.of(b1, b2), REMOVE_ME, true, true),
                new Timeline(List.of(c1), REMOVE_ME, true, true)
        )), common1, common2);

        assertEquals(expected, chain);
    }

    @Test
    public void converging2Paths() {
        TConn a1 = new TConn(REMOVE_ME, "A1");
        TConn a2 = new TConn(REMOVE_ME, "A2");
        TConn b1 = new TConn(REMOVE_ME, "B1");
        TConn b2 = new TConn(REMOVE_ME, "B2");
        TConn c1 = new TConn(REMOVE_ME, "C1");
        TConn common1 = new TConn(REMOVE_ME, "COMMON1");
        TConn common2 = new TConn(REMOVE_ME, "COMMON2");
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        List<TConn> expected = List.of(new TConn(REMOVE_ME, List.of(
                new Timeline(List.of(a1, a2), REMOVE_ME, true, true),
                new Timeline(List.of(b1, b2), REMOVE_ME, true, true)
        )), common1, common2);

        assertEquals(expected, chain);
    }

    @Test
    public void converging2PathsConvergeEarlier() {
        TConn a1 = new TConn(REMOVE_ME, "A1");
        TConn a2 = new TConn(REMOVE_ME, "A2");
        TConn b1 = new TConn(REMOVE_ME, "B1");
        TConn b2 = new TConn(REMOVE_ME, "B2");
        TConn c1 = new TConn(REMOVE_ME, "C1");
        TConn common1 = new TConn(REMOVE_ME, "COMMON1");
        TConn common2 = new TConn(REMOVE_ME, "COMMON2");
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1, common2),
                List.of(c1, common2)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        Timeline aTimeline = new Timeline(List.of(a1, a2), REMOVE_ME, true, true);
        Timeline bTimeline = new Timeline(List.of(b1, b2), REMOVE_ME, true, true);
        Timeline cTimeline = new Timeline(List.of(c1), REMOVE_ME, true, true);

        Timeline p2 = new Timeline(List.of(new TConn(REMOVE_ME, List.of(
                aTimeline,
                bTimeline
        )), common1), REMOVE_ME, true, true);

        TConn p1 = new TConn(REMOVE_ME, List.of(p2, cTimeline));

        List<TConn> expected = List.of(p1, common2);

        assertEquals(2, chain.size());
        assertEquals(p2, ((TParallel)chain.get(0).node()).timelines().get(0));
        assertEquals(cTimeline, ((TParallel)chain.get(0).node()).timelines().get(1)); // TODO should be cTimeline - but same as p2
        assertEquals(p1, chain.get(0));
        assertEquals(expected, chain);
    }
}