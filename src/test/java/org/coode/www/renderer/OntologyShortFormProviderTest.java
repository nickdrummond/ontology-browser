package org.coode.www.renderer;

import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;

import static org.junit.Assert.*;

public class OntologyShortFormProviderTest {

    @Test
    public void getShortFormForRootReturnsDefault() throws OWLOntologyCreationException {
        OWLOntology ont = new OWLManager().get().createOntology();
        OntologyShortFormProvider provider = new OntologyShortFormProvider(ont);
        assertEquals("All ontologies", provider.getShortForm(ont));
    }

    @Test
    public void getShortForm() throws OWLOntologyCreationException {
        OWLOntologyManager mngr = new OWLManager().get();
        OWLOntology ont = mngr.createOntology();
        String expected = "http://example.com/ont2";
        OWLOntology ont2 = mngr.createOntology(new OWLOntologyID(IRI.create(expected)));

        mngr.applyChanges(new AddImport(ont, mngr.getOWLDataFactory().getOWLImportsDeclaration(ont2.getOntologyID().getOntologyIRI().get())));

        OntologyShortFormProvider provider = new OntologyShortFormProvider(ont);
        assertEquals("ont2", provider.getShortForm(ont2));
    }

    @Test
    public void getShortFormForAnonymousOntologiesReturnsShortenedDocIRI() throws OWLOntologyCreationException {
        OWLOntologyManager mngr = new OWLManager().get();
        OWLOntology ont = mngr.createOntology();
        String expected = "http://example.com/ontLocation";
        OWLOntology ont2 = mngr.createOntology();
        mngr.setOntologyDocumentIRI(ont2, IRI.create(expected));

        mngr.applyChanges(new AddImport(ont, mngr.getOWLDataFactory().getOWLImportsDeclaration(IRI.create(expected))));

        OntologyShortFormProvider provider = new OntologyShortFormProvider(ont);
        assertEquals("ontLocation", provider.getShortForm(ont2));
    }
}