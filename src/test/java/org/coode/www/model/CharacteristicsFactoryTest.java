package org.coode.www.model;

import org.coode.www.renderer.UsageVisibilityVisitor;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class CharacteristicsFactoryTest {

    private final IRI ontologyIRI = IRI.create("http://example.com");

    private CharacteristicsFactory fac;
    private OWLOntologyManager mngr;

    private OWLClassAssertionAxiom classAssertionAxiom;
    private OWLAnnotationAssertionAxiom annotationAssertionAxiom;
    private OWLNamedIndividual ind;
    private OWLClass cls;
    private OWLAnnotationProperty prop;

    @Mock
    private Comparator<OWLObject> comparator;

    @Mock
    UsageVisibilityVisitor usageVisibilityVisitor;

    @Before
    public void setup() {
        fac = new CharacteristicsFactory();
        mngr = new OWLManager().get();
        OWLDataFactory df = mngr.getOWLDataFactory();

        prop = df.getOWLAnnotationProperty("http://example.com/#p");
        ind = df.getOWLNamedIndividual("http://example.com/#i");
        cls = df.getOWLClass("http://example.com/#C");

        OWLAnnotationSubject subj = cls.getIRI();
        OWLAnnotationValue val = ind.getIRI().annotationValue();

        annotationAssertionAxiom = df.getOWLAnnotationAssertionAxiom(prop, subj, val);
        classAssertionAxiom = df.getOWLClassAssertionAxiom(cls, ind);

        // we're not testing the filter here so always allow axioms through
        when(usageVisibilityVisitor.getShowUsage(any(), any())).thenReturn(true);
    }

    @Test
    public void getUsageReturnsAxioms() throws OWLOntologyCreationException {
        OWLOntology ontology = mngr.createOntology(Set.of(classAssertionAxiom), ontologyIRI);

        Optional<Characteristic> result = fac.getUsage(ind, Set.of(ontology), comparator, usageVisibilityVisitor);

        assertTrue(result.isPresent());
        Optional<OWLObjectWithOntology> axiomAndOnt = result.flatMap(ch -> ch.getObjects().stream().findFirst());
        assertTrue(axiomAndOnt.isPresent());

        assertEquals(classAssertionAxiom, axiomAndOnt.get().getOWLObject());
        assertEquals(ontology, axiomAndOnt.get().getOWLOntology());
    }

    @Test
    public void getUsageReturnsAnnotationsWithEntityIRIAsValue() throws OWLOntologyCreationException {
        OWLOntology ontology = mngr.createOntology(Set.of(annotationAssertionAxiom), ontologyIRI);

        Optional<Characteristic> result = fac.getUsage(ind, Set.of(ontology), comparator, usageVisibilityVisitor);

        assertTrue(result.isPresent());
        Optional<OWLObjectWithOntology> axiomAndOnt = result.flatMap(ch -> ch.getObjects().stream().findFirst());
        assertTrue(axiomAndOnt.isPresent());

        assertEquals(annotationAssertionAxiom, axiomAndOnt.get().getOWLObject());
        assertEquals(ontology, axiomAndOnt.get().getOWLOntology());
    }
}