package org.ontbrowser.www.renderer;

import org.junit.Test;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import static org.junit.Assert.*;

public class OntologyShortFormProviderTest {

    private IRI labelIRI = OWLRDFVocabulary.RDFS_LABEL.getIRI();

    @Test
    public void getShortForm() throws OWLOntologyCreationException {
        OWLOntologyManager mngr = new OWLManager().get();
        OWLDataFactory df = mngr.getOWLDataFactory();

        OWLOntology ont = mngr.createOntology();
        String expected = "http://example.com/ont2";
        OWLOntology ont2 = mngr.createOntology(new OWLOntologyID(IRI.create(expected)));

        OWLImportsDeclaration importsDecl = df.getOWLImportsDeclaration(ont2.getOntologyID().getOntologyIRI().orElseThrow());
        mngr.applyChanges(new AddImport(ont, importsDecl));

        OntologyShortFormProvider provider = new OntologyShortFormProvider(labelIRI);
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

        OntologyShortFormProvider provider = new OntologyShortFormProvider(labelIRI);
        assertEquals("ontLocation", provider.getShortForm(ont2));
    }

    @Test
    public void getShortFormWithLabel() throws OWLOntologyCreationException {
        OWLOntologyManager mngr = new OWLManager().get();
        OWLDataFactory df = mngr.getOWLDataFactory();

        OWLOntology ont = mngr.createOntology(IRI.create("http://example.com/"));
        String expected = "an ontology";

        OWLAnnotation labelAnnotation = df.getOWLAnnotation(df.getOWLAnnotationProperty(labelIRI), df.getOWLLiteral(expected));
        mngr.applyChanges(new AddOntologyAnnotation(ont, labelAnnotation));

        OntologyShortFormProvider provider = new OntologyShortFormProvider(labelIRI);
        assertEquals(expected, provider.getShortForm(ont));
    }
}