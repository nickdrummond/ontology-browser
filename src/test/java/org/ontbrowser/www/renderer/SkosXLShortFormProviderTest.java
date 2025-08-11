package org.ontbrowser.www.renderer;

import org.eclipse.rdf4j.model.vocabulary.SKOSXL;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.SKOSVocabulary;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

@ExtendWith(MockitoExtension.class)
public class SkosXLShortFormProviderTest {

    private OWLOntology ont;
    private OWLDataFactory df;
    private OWLClass skosConcept;
    private OWLAnnotationProperty prefLabel;
    private OWLAnnotationProperty literalForm;
    private SkosXLShortFormProvider provider;

    private OWLNamedIndividual concept;

    @Mock
    private ShortFormProvider mockBackupSfp;

    @BeforeEach
    public void setup() throws OWLOntologyCreationException {

        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        df = mngr.getOWLDataFactory();
        ont = mngr.createOntology();

        prefLabel = df.getOWLAnnotationProperty(SKOSXL.PREF_LABEL.toString());
        literalForm = df.getOWLAnnotationProperty(SKOSXL.LITERAL_FORM.toString());

        skosConcept = df.getOWLClass(SKOSVocabulary.CONCEPT);
        concept = df.getOWLNamedIndividual("conceptIRI");
        provider = new SkosXLShortFormProvider("en", Set.of(ont), mockBackupSfp);
    }

    @Test
    public void rendersNonSkosConceptsUsingDelegate() {
        OWLNamedIndividual label = df.getOWLNamedIndividual("labelIRI");
        ont.addAxioms(
                df.getOWLAnnotationAssertionAxiom(concept.getIRI(), df.getOWLAnnotation(prefLabel, label.getIRI())),
                df.getOWLAnnotationAssertionAxiom(label.getIRI(), df.getOWLAnnotation(literalForm, df.getOWLLiteral("expected")))
        );

        provider.getShortForm(concept);
        verify(mockBackupSfp, times(1)).getShortForm(concept);
    }

    @Test
    public void rendersSkosConceptsUsingNamesLabels() {
        OWLNamedIndividual label = df.getOWLNamedIndividual("labelIRI");
        ont.addAxioms(
                df.getOWLClassAssertionAxiom(skosConcept, concept),
                df.getOWLAnnotationAssertionAxiom(concept.getIRI(), df.getOWLAnnotation(prefLabel, label.getIRI())),
                df.getOWLAnnotationAssertionAxiom(label.getIRI(), df.getOWLAnnotation(literalForm, df.getOWLLiteral("expected")))
        );

        assertEquals("expected", provider.getShortForm(concept));
    }

    @Test
    public void rendersSkosConceptsUsingAnonymousLabels_matchesLang() {
        OWLAnonymousIndividual label = df.getOWLAnonymousIndividual();
        ont.addAxioms(
                df.getOWLClassAssertionAxiom(skosConcept, concept),
                df.getOWLAnnotationAssertionAxiom(concept.getIRI(), df.getOWLAnnotation(prefLabel, label)),
                df.getOWLAnnotationAssertionAxiom(label, df.getOWLAnnotation(literalForm, df.getOWLLiteral("expected", "en")))
        );

        assertEquals("expected", provider.getShortForm(concept));
    }

    @Test
    public void rendersSkosConceptsUsingAnonymousLabels_defaultsToNoLang() {
        OWLAnonymousIndividual label = df.getOWLAnonymousIndividual();
        ont.addAxioms(
                df.getOWLClassAssertionAxiom(skosConcept, concept),
                df.getOWLAnnotationAssertionAxiom(concept.getIRI(), df.getOWLAnnotation(prefLabel, label)),
                df.getOWLAnnotationAssertionAxiom(label, df.getOWLAnnotation(literalForm, df.getOWLLiteral("expected")))
        );

        assertEquals("expected", provider.getShortForm(concept));
    }
}