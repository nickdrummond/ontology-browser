package org.coode.www.service;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLEntity;

import javax.annotation.Nonnull;
import java.util.List;
import java.util.Set;

public interface SearchService {
    List<OWLEntity> findByName(String input, OWLHTMLKit kit);

    List<AxiomWithMetadata> findByAnnotation(@Nonnull String value,
                                             OWLAnnotationProperty searchProp,
                                             @Nonnull OWLHTMLKit kit);

    Set<OWLEntity> getEntities(IRI iri, OWLHTMLKit kit);
}
