package org.coode.www;

import org.coode.www.formatter.OntologyIdFormatter;
import org.junit.Test;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.util.Locale;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;

public class OntologyIdFormatterTest {

    private final OntologyIdFormatter formatter = new OntologyIdFormatter();

    @Test
    public void shouldRenderAnonymousIfTheOntologyHasNoIri() {
        OWLOntologyID id = new OWLOntologyID();

        assertThat(formatter.print(id, Locale.ENGLISH), equalTo("Anonymous"));
    }
}
