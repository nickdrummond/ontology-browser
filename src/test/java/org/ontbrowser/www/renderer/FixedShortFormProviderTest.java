package org.ontbrowser.www.renderer;

import org.junit.Ignore;
import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.util.SimpleIRIShortFormProvider;
import uk.ac.manchester.cs.owl.owlapi.OWLNamedIndividualImpl;

import static org.junit.Assert.assertEquals;

public class FixedShortFormProviderTest {

    private ShortFormProvider sfp = new FixedSimpleShortFormProvider(new SimpleIRIShortFormProvider());

    @Test
    public void shouldDealWithNamesEndingWithSlash() {
        IRI iri = IRI.create("http://null.com/ontology/monkey/");
        var ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("monkey", label);
    }

    @Test
    public void shouldDealWithNamesEndingHash() {
        IRI iri = IRI.create("http://null.com/ontology/monkey#");
        var ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("monkey", label);
    }

    @Ignore
    @Test
    public void shouldDealWithNamesStartingWithNumbers() {
        IRI iri = IRI.create("http://null.com/ontology.owl#501st");
        var ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("501st", label);
    }

    @Ignore
    @Test
    public void shouldDealWithNamesWithApostrophes() {
        IRI iri = IRI.create("http://null.com/ontology.owl#D'Qar");
        var ind = new OWLNamedIndividualImpl(iri);
        String label = sfp.getShortForm(ind);
        assertEquals("D'Qar", label);
    }
}
