package org.coode.www.repository;

import org.junit.Test;
import org.semanticweb.owlapi.model.IRI;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class IRIReadConverterTest {

    private IRIReadConverter reader = new IRIReadConverter();

    @Test
    public void write() {
        String str = "http://example.com";
        IRI expected = IRI.create(str);
        assertThat(reader.convert(str), equalTo(expected));
    }
}
