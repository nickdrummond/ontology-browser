package org.coode.www;

import com.google.common.base.Optional;
import org.coode.www.formatter.OntologyIdFormatter;
import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;

import java.util.Locale;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class OntologyIdFormatterTest {

    private final OntologyIdFormatter formatter = new OntologyIdFormatter();

    @Test
    public void shouldRenderTheVersionIriIfTheOntologyHasOne() {
        String expected = "http://example.com/thing/2";
        IRI iri = IRI.create("http://example.com/thing");
        IRI versionIri = IRI.create(expected);
        OWLOntologyID id = new OWLOntologyID(Optional.of(iri), Optional.of(versionIri));

        assertThat(formatter.print(id, Locale.ENGLISH), equalTo("thing (http://example.com/thing/2)"));
    }

    @Test
    public void shouldRenderTheIriIfTheOntologyHasOne() {
        String expected = "http://example.com/thing";
        IRI iri = IRI.create(expected);
        OWLOntologyID id = new OWLOntologyID(Optional.of(iri), Optional.<IRI>absent());

        assertThat(formatter.print(id, Locale.ENGLISH), equalTo("thing (http://example.com/thing)"));
    }

    @Test
    public void shouldRenderAnonymousIfTheOntologyHasNoIri() {
        OWLOntologyID id = new OWLOntologyID();

        assertThat(formatter.print(id, Locale.ENGLISH), equalTo("Anonymous"));
    }
}
