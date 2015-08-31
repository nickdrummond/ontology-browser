package org.coode.www.repository;

import org.semanticweb.owlapi.model.IRI;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.WritingConverter;

@WritingConverter
public class IRIWriteConverter implements Converter<IRI, String> {
    @Override
    public String convert(IRI iri) {
        return iri.toString();
    }
}
