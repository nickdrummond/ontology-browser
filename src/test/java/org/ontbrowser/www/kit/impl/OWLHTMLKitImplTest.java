package org.ontbrowser.www.kit.impl;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;

import static org.junit.Assert.*;

@RunWith(MockitoJUnitRunner.class)
public class OWLHTMLKitImplTest {

    private OWLHTMLKitImpl kit;

    @Mock
    private OWLOntologyManager mngr;

    @Mock
    private OntologyIRIShortFormProvider ontologyRenderer;

    private final String rootBase = "http://example.com/root";
    private final IRI locationIRI = IRI.create("file:///ont.owl");

    private final Set<OWLOntology> ontologies = new HashSet<>();

    private OWLOntology rootOnt;

    @Before
    public void setup() {
        rootOnt = mock(OWLOntology.class);

        ontologies.add(rootOnt);

        when(mngr.getOntologies()).thenReturn(ontologies);
        when(mngr.getOntologyDocumentIRI(rootOnt)).thenReturn(locationIRI);
        when(rootOnt.getOntologyID()).thenReturn(new OWLOntologyID(IRI.create(rootBase), IRI.create(rootBase + "/1")));

        kit = new OWLHTMLKitImpl(mngr, rootOnt, ontologyRenderer);
    }


    @Test
    public void getOntologyForIRIReturnsEmptyWhenNotFound() {

        Optional<OWLOntology> result = kit.getOntologyForIRI(IRI.create(rootBase + "/2"));
        assertTrue(result.isEmpty());
    }

    @Test
    public void getOntologyForIRIReturnsOntologyByIRI() {

        Optional<OWLOntology> result = kit.getOntologyForIRI(IRI.create(rootBase));
        assertTrue(result.isPresent());
        assertEquals(rootOnt, result.get());
    }

    @Test
    public void getOntologyForIRIReturnsOntologyByVersionIRI() {

        Optional<OWLOntology> result = kit.getOntologyForIRI(IRI.create(rootBase + "/1"));
        assertTrue(result.isPresent());
        assertEquals(rootOnt, result.get());
    }

    @Test
    public void getOntologyForIRIReturnsOntologyByLocation() {

        Optional<OWLOntology> result = kit.getOntologyForIRI(locationIRI);
        assertTrue(result.isPresent());
        assertEquals(rootOnt, result.get());
    }

    @Test
    public void getOntologies() {
        // simply passes the ontologies through
        assertEquals(ontologies, kit.getOntologies());
    }
}