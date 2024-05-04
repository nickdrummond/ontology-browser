package org.ontbrowser.www.model;

import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import uk.ac.manchester.cs.owl.owlapi.OWLClassImpl;

import static org.junit.Assert.*;

public class DLQueryTest {

    @Test
    public void equalityUsesBothExpressionAndQueryType() {
        DLQuery q1 = new DLQuery(new OWLClassImpl(IRI.create("a")), QueryType.instances);
        DLQuery q2 = new DLQuery(new OWLClassImpl(IRI.create("a")), QueryType.instances);

        assertEquals(q1, q2);
    }

    @Test
    public void differentQueryTypeDifferentQuery() {
        DLQuery q1 = new DLQuery(new OWLClassImpl(IRI.create("a")), QueryType.instances);
        DLQuery q2 = new DLQuery(new OWLClassImpl(IRI.create("a")), QueryType.ancestors);

        assertNotEquals(q1, q2);
    }

    @Test
    public void differentClassExpressionDifferentQuery() {
        DLQuery q1 = new DLQuery(new OWLClassImpl(IRI.create("a")), QueryType.instances);
        DLQuery q2 = new DLQuery(new OWLClassImpl(IRI.create("b")), QueryType.instances);

        assertNotEquals(q1, q2);
    }
}