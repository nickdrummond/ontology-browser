package org.coode.www.model.timeline;

import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import uk.ac.manchester.cs.owl.owlapi.OWLNamedIndividualImpl;

import java.util.ArrayList;
import java.util.List;

import static org.coode.www.service.timeline.EventUtils.REMOVE_ME;
import static org.coode.www.service.timeline.EventUtils.buildConverging;
import static org.junit.Assert.assertEquals;

public class EventUtilsTest {

    TConn a1 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("A1")));
    TConn a2 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("A2")));
    TConn b1 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("B1")));
    TConn b2 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("B2")));
    TConn c1 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("C1")));
    TConn common1 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("COMMON1")));
    TConn common2 = new TConn(REMOVE_ME, new OWLNamedIndividualImpl(IRI.create("COMMON2")));

    @Test
    public void diverging() {
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

        TConn p1 = new TConn(REMOVE_ME, List.of(cTimeline, p2));

        List<TConn> expected = List.of(p1, common2);

        assertEquals(2, chain.size());
        assertEquals(p2, ((TParallel)chain.get(0).node()).timelines().get(1));
        assertEquals(cTimeline, ((TParallel)chain.get(0).node()).timelines().get(0)); // TODO should be cTimeline - but same as p2
        assertEquals(p1, chain.get(0));
        assertEquals(expected, chain);
    }


    @Test
    public void convergingPartway() {
        List<List<TConn>> divergentChains = List.of(
                List.of(a1, a2, common1, common2),
                List.of(b1, b2, common1)
        );

        List<TConn> chain = buildConverging(new ArrayList<>(), divergentChains);

        Timeline aTimeline = new Timeline(List.of(a1, a2), REMOVE_ME, true, true);
        Timeline bTimeline = new Timeline(List.of(b1, b2), REMOVE_ME, true, true);

        Timeline p2 = new Timeline(List.of(new TConn(REMOVE_ME, List.of(
                aTimeline,
                bTimeline
        )), common1, common2), REMOVE_ME, true, true);

        assertEquals(1, chain.size());
        assertEquals(aTimeline, ((TParallel)chain.get(0).node()).timelines().get(0));
        assertEquals(bTimeline, ((TParallel)chain.get(0).node()).timelines().get(1));
        assertEquals(p2, chain.get(0));
//        assertEquals(expected, chain);
    }
}