package org.coode.www.repository;

import org.semanticweb.owlapi.model.IRI;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.ReadingConverter;

@ReadingConverter
public class IRIReadConverter implements Converter<String, IRI> {
    @Override
    public IRI convert(String string) {
        return IRI.create(string);
    }
}
