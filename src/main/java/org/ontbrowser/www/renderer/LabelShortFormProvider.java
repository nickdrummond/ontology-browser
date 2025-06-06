package org.ontbrowser.www.renderer;

import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Collections;
import java.util.Set;

import static org.ontbrowser.www.renderer.Lang.createLangMap;

/**
 * A shortformProvider that uses the server properties to render
 * 1) An annotation value on the label (in the given language), if available otherwise
 * 2) uses the default shortFormProvider provided
 */
public class LabelShortFormProvider implements ShortFormProvider {

    private final AnnotationValueShortFormProvider delegate;

    public LabelShortFormProvider(final OWLAnnotationProperty annotProp,
                                  final String lang,
                                  final Set<OWLOntology> ontologies,
                                  final ShortFormProvider defaultSFP) {
        delegate = new AnnotationValueShortFormProvider(
                Collections.singletonList(annotProp),
                createLangMap(annotProp, lang),
                ontologies::stream,
                defaultSFP);
    }

    public String getShortForm(OWLEntity owlEntity) {
        return delegate.getShortForm(owlEntity);
    }

    @Override
    public void dispose() {
        delegate.dispose();
    }
}
