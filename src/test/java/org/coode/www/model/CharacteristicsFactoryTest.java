package org.coode.www.model;

import org.coode.www.renderer.UsageVisibilityVisitor;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.*;

import static org.junit.Assert.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class CharacteristicsFactoryTest {

    private final IRI ontologyIRI = IRI.create("http://example.com");

    private CharacteristicsFactory fac;
    private OWLOntologyManager mngr;
    private OWLDataFactory df;

    private OWLNamedIndividual ind;
    private OWLClass cls;
    private OWLAnnotationProperty prop, prop2;

    @Mock
    private Comparator<OWLObject> comparator;

    @Mock
    private UsageVisibilityVisitor usageVisibilityVisitor;


    @Before
    public void setup() {
        fac = new CharacteristicsFactory();
        mngr = new OWLManager().get();
        df = mngr.getOWLDataFactory();

        prop = df.getOWLAnnotationProperty("http://example.com/#p");
        prop2 = df.getOWLAnnotationProperty("http://example.com/#p2");
        ind = df.getOWLNamedIndividual("http://example.com/#i");
        cls = df.getOWLClass("http://example.com/#C");

        // we're not testing the filter here so always allow axioms through
        when(usageVisibilityVisitor.getShowUsage(any(), any())).thenReturn(true);
    }

    @Test
    public void getUsageReturnsAxioms() throws OWLOntologyCreationException {
        OWLAxiom clsAssertionAxiom = df.getOWLClassAssertionAxiom(cls, ind);

        OWLOntology ontology = mngr.createOntology(Set.of(clsAssertionAxiom), ontologyIRI);

        Optional<Characteristic> result = fac.getUsage(ind, Set.of(ontology), comparator, usageVisibilityVisitor);

        assertTrue(result.isPresent());
        Optional<OWLObjectWithOntology> axiomAndOnt = result.flatMap(ch -> ch.getObjects().stream().findFirst());
        assertTrue(axiomAndOnt.isPresent());

        assertEquals(clsAssertionAxiom, axiomAndOnt.get().getOWLObject());
        assertEquals(ontology, axiomAndOnt.get().getOWLOntology());
    }

    @Test
    public void getUsageReturnsAnnotationsWithEntityIRIAsValue() throws OWLOntologyCreationException {
        OWLAxiom clsAnnotationAssertionAxiom = df.getOWLAnnotationAssertionAxiom(prop, cls.getIRI(), ind.getIRI().annotationValue());

        OWLOntology ontology = mngr.createOntology(Set.of(clsAnnotationAssertionAxiom), ontologyIRI);

        Optional<Characteristic> result = fac.getUsage(ind, Set.of(ontology), comparator, usageVisibilityVisitor);

        assertTrue(result.isPresent());
        Optional<OWLObjectWithOntology> axiomAndOnt = result.flatMap(ch -> ch.getObjects().stream().findFirst());
        assertTrue(axiomAndOnt.isPresent());

        assertEquals(clsAnnotationAssertionAxiom, axiomAndOnt.get().getOWLObject());
        assertEquals(ontology, axiomAndOnt.get().getOWLOntology());
    }

    @Test
    public void getAnnotationCharacteristicsReturnsAnnotationsLabeledUsingProvider() throws OWLOntologyCreationException {
        String expectedLabel = "expected label";
        OWLLiteral expectedValue = df.getOWLLiteral("an annotation");

        OWLAxiom indAnnotation = df.getOWLAnnotationAssertionAxiom(prop, ind.getIRI(), expectedValue.annotationValue());
        OWLOntology ontology = mngr.createOntology(Set.of(indAnnotation), ontologyIRI);

        ShortFormProvider sfp = mock(ShortFormProvider.class);
        when(sfp.getShortForm(any())).thenReturn(expectedLabel);

        List<Characteristic> result = fac.getAnnotationCharacteristics(ind, Set.of(ontology), comparator, sfp);

        assertEquals(1, result.size()); // single annotation
        assertEquals(expectedLabel, result.get(0).getName()); // labelled correctly

        List<OWLObjectWithOntology> actualValues = result.get(0).getObjects();

        assertEquals(1, actualValues.size()); // single value
        assertEquals(new OWLObjectWithOntology(expectedValue, ontology), actualValues.get(0)); // value wrapped with ont
    }

    @Test
    public void getAnnotationCharacteristicsReturnsMultipleCharacteristics() throws OWLOntologyCreationException {
        OWLLiteral firstExpected = df.getOWLLiteral("1st");
        OWLLiteral secondExpected = df.getOWLLiteral("2nd");

        OWLAxiom indAnnotation = df.getOWLAnnotationAssertionAxiom(prop, ind.getIRI(), firstExpected.annotationValue());
        OWLAxiom indAnnotation3 = df.getOWLAnnotationAssertionAxiom(prop2, ind.getIRI(), secondExpected.annotationValue());

        OWLOntology ontology = mngr.createOntology(Set.of(indAnnotation, indAnnotation3), ontologyIRI);

        // sorted (prop, prop2)
        when(comparator.compare(prop, prop2)).thenReturn(-1);
        when(comparator.compare(prop2, prop)).thenReturn(1);

        ShortFormProvider sfp = mock(ShortFormProvider.class);
        when(sfp.getShortForm(prop)).thenReturn("p");
        when(sfp.getShortForm(prop2)).thenReturn("p2");

        List<Characteristic> result = fac.getAnnotationCharacteristics(ind, Set.of(ontology), comparator, sfp);

        // each annotation property is a separate characteristic
        assertEquals(2, result.size());

        // 1st characteristic
        assertEquals("p", result.get(0).getName());
        List<OWLObjectWithOntology> propValues = result.get(0).getObjects();
        assertEquals(1, propValues.size());
        assertEquals(new OWLObjectWithOntology(firstExpected, ontology), propValues.get(0)); // value wrapped with ont

        // 2nd characteristic
        assertEquals("p2", result.get(1).getName());
        List<OWLObjectWithOntology> prop2Values = result.get(1).getObjects();
        assertEquals(1, prop2Values.size());
        assertEquals(new OWLObjectWithOntology(secondExpected, ontology), prop2Values.get(0)); // value wrapped with ont
    }
}