package org.coode.www.repository;

import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class IRIWriteConverterTest {

    private IRIWriteConverter writer = new IRIWriteConverter();

    @Test
    public void write() {
        String expected = "http://example.com";
        IRI iri = IRI.create(expected);
        assertThat(writer.convert(iri), equalTo(expected));
    }
}
