package org.coode.www.model.timeline;

import junit.framework.TestCase;
import org.junit.Test;

public class TEventTest extends TestCase {

    @Test
    public void testGetClass() {
        TNode node = new TEvent("label");
        String name = node.getClass().getSimpleName();
        assertEquals("TEvent", name);
    }
}