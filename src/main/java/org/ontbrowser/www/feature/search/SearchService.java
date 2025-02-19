package org.ontbrowser.www.feature.search;

import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLEntity;

import javax.annotation.Nonnull;
import java.util.List;

public interface SearchService {

    List<OWLEntity> findByName(String input, OWLHTMLKit kit);

    List<AxiomWithMetadata> findByAnnotation(@Nonnull String value,
                                             OWLAnnotationProperty searchProp,
                                             @Nonnull OWLHTMLKit kit);
}
