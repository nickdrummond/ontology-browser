package org.coode.www.model;

import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.IndividualCharacteristicsBuilder;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import java.util.*;

import static org.junit.Assert.*;

@RunWith(MockitoJUnitRunner.class)
public class OntologyCharacteristicsBuilderTest {

    private final IRI ontologyIRI = IRI.create("http://example.com");

    private OWLOntologyManager mngr;
    private OWLDataFactory df;

    private OWLNamedIndividual ind;
    private OWLClass cls;

    @Mock
    private Comparator<OWLObject> comparator;

    @Before
    public void setup() {
        mngr = new OWLManager().get();
        df = mngr.getOWLDataFactory();

        ind = df.getOWLNamedIndividual("http://example.com/#i");
        cls = df.getOWLClass("http://example.com/#C");
    }

    @Test
    public void getIndividualTypesReturnsCharacteristic() throws OWLOntologyCreationException {
        OWLAxiom clsAssertionAxiom = df.getOWLClassAssertionAxiom(cls, ind);

        OWLOntology ont = mngr.createOntology(Set.of(clsAssertionAxiom), ontologyIRI);

        List<Characteristic> result = new IndividualCharacteristicsBuilder(
                ind, ont, comparator, Collections.emptyList(), 10).getCharacteristics();

        // Types characteristic
        assertEquals(1, result.size());
        assertEquals("Types", result.get(0).getName());

        // With the wrapped OWLClass, along with axiom and ont
        AxiomWithMetadata axiomAndOnt = result.get(0).getObjects().get(0);
        assertEquals(cls, axiomAndOnt.getOWLObject());
        assertEquals(clsAssertionAxiom, axiomAndOnt.getOWLAxiom());
        assertEquals(ont, axiomAndOnt.getOWLOntology());
    }
}