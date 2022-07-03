package org.coode.www.renderer;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import javax.annotation.Nonnull;
import java.util.*;

/**
 * A shortformProvider that uses the server properties to render
 * 1) An annotation value on the label (in the given language), if available otherwise
 * 2) uses the default shortFormProvider provided
 */
public class LabelShortFormProvider implements ShortFormProvider {

    private AnnotationValueShortFormProvider delegate;

    public LabelShortFormProvider(final OWLAnnotationProperty annotProp,
                                  final String lang,
                                  final Set<OWLOntology> ontologies,
                                  final ShortFormProvider defaultSFP) {

        // the annotation label sfp
        delegate = new AnnotationValueShortFormProvider(
                Collections.singletonList(annotProp),
                createLangMap(annotProp, lang),
                ontologies::stream,
                defaultSFP);
    }

    @Nonnull
    public String getShortForm(@Nonnull OWLEntity owlEntity) {
        return delegate.getShortForm(owlEntity);
    }

    public void dispose() {
        delegate.dispose();
    }

    private <P> Map<P, List<String>> createLangMap(P p, String lang) {
        final Map<P, List<String>> lMap = new HashMap<P, List<String>>();
        if (lang.length() > 0){
            List<String> langs = new ArrayList<String>();
            langs.add(lang);
            langs.add(""); // default to no language
            lMap.put(p, langs);
        }
        return lMap;
    }
}
