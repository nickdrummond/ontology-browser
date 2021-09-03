package org.coode.www.renderer;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.ServerConfig;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.PropertyAssertionValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.stream.Stream;

/**
 * A shortformProvider that uses the server properties to render
 * 1) An annotation value on the label (in the given language), if available otherwise
 * 2) A property value on the label property (again, given the language), otherwise
 * 3) uses the default shortFormProvider provided
 */
public class LabelShortFormProvider implements ShortFormProvider {

    private AnnotationValueShortFormProvider delegate;

    public LabelShortFormProvider(final OWLHTMLKit kit, final ShortFormProvider defaultSFP) {

        ServerConfig config = kit.getConfig();

        final String lang = config.getLabelLang();

        final OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        final OWLOntologySetProvider activeOntologiesSetProvider = () -> kit.getActiveOntologies().stream();

        // the property assertion sfp
        OWLDataProperty dataProp = df.getOWLDataProperty(config.getLabelPropertyIri());
        ShortFormProvider pValueProvider =
                new PropertyAssertionValueShortFormProvider(
                        Collections.<OWLPropertyExpression>singletonList(dataProp),
                        createLangMap((OWLDataPropertyExpression)dataProp, lang),
                        activeOntologiesSetProvider,
                        defaultSFP);

        // the annotation label sfp
        OWLAnnotationProperty annotProp = df.getOWLAnnotationProperty(config.getLabelAnnotationIri());
        delegate = new AnnotationValueShortFormProvider(
                Collections.singletonList(annotProp),
                createLangMap(annotProp, lang),
                activeOntologiesSetProvider,
                pValueProvider);
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
