package org.ontbrowser.www.model;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.IndividualCharacteristicsBuilder;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;


public class OntologyCharacteristicsBuilderTest {

    private final IRI ontologyIRI = IRI.create("http://example.com");

    private OWLOntologyManager mngr;
    private OWLDataFactory df;

    private OWLNamedIndividual ind;
    private OWLClass cls;

    @Mock
    private Comparator<OWLObject> comparator;

    @BeforeEach
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
        assertEquals(cls, axiomAndOnt.owlObject());
        assertEquals(clsAssertionAxiom, axiomAndOnt.owlAxiom());
        assertEquals(ont, axiomAndOnt.owlOntology());
    }
}