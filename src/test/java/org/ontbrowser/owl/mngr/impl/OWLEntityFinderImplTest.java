package org.ontbrowser.owl.mngr.impl;

import org.junit.jupiter.api.Test;
import org.ontbrowser.www.kit.impl.OWLEntityFinderImpl;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;

import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;


public class OWLEntityFinderImplTest {

    @Test
    public void getEntities() throws OWLOntologyCreationException {
        OWLOntologyManager mngr = OWLManager.createOWLOntologyManager();
        OWLDataFactory df = mngr.getOWLDataFactory();
        OWLOntology ont = mngr.createOntology();

        CachingBidirectionalShortFormProvider cache = new CachingBidirectionalShortFormProvider() {
            @Override
            protected String generateShortForm(OWLEntity owlEntity) {
                return owlEntity.getIRI().toString();
            }
        };

        OWLEntityFinderImpl finder = new OWLEntityFinderImpl(cache, df);

        OWLClass helping_at_the = cls("Helping_at_the", df, ont, cache);
        OWLClass also_helping_at_the = cls("Also_helping_at_the", df, ont, cache);
        OWLClass has_the_word = cls("Has_the_word_help", df, ont, cache);
        OWLClass also_has_the = cls("Also_has_the_word_help", df, ont, cache);

        Set<OWLEntity> results = finder.getOWLEntities(".*Helping.*");

        assertEquals(Set.of(helping_at_the, also_helping_at_the), results);
    }

    private OWLClass cls(String classIRI, OWLDataFactory df, OWLOntology ont, CachingBidirectionalShortFormProvider cache) {
        OWLClass cls = df.getOWLClass(classIRI);
        ont.addAxiom(df.getOWLDeclarationAxiom(cls));
        cache.add(cls);
        return cls;
    }
}