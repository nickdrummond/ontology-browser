package org.coode.www.renderer;

import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.util.ShortFormProvider;
import uk.ac.manchester.cs.owl.owlapi.OWLNamedIndividualImpl;

import static org.junit.Assert.*;

public class FixedShortFormProviderTest {

    @Test
    public void shouldDealWithNamesEndingWithSlash() {
        ShortFormProvider sfp = new FixedSimpleShortFormProvider();
        IRI iri = IRI.create("http://null.com/ontology/monkey/");
        OWLNamedIndividual ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("monkey", label);
    }

    @Test
    public void shouldDealWithNamesEndingHash() {
        ShortFormProvider sfp = new FixedSimpleShortFormProvider();
        IRI iri = IRI.create("http://null.com/ontology/monkey#");
        OWLNamedIndividual ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("monkey", label);
    }

    @Test
    public void shouldDealWithNamesStartingWithNumbers() {
        ShortFormProvider sfp = new FixedSimpleShortFormProvider();
        IRI iri = IRI.create("http://null.com/ontology.owl#501st");
        OWLNamedIndividual ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("501st", label);
    }

    @Test
    public void shouldDealWithNamesWithApostrophes() {
        ShortFormProvider sfp = new FixedSimpleShortFormProvider();
        IRI iri = IRI.create("http://null.com/ontology.owl#D'Qar");
        OWLNamedIndividual ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("D'Qar", label);
    }
}
