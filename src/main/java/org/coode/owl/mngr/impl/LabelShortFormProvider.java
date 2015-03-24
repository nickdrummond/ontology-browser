package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.ServerProperty;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.AnnotationValueShortFormProvider;
import org.semanticweb.owlapi.util.PropertyAssertionValueShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.*;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 2, 2011<br><br>
 *
 * A shortformProvider that uses the server properties to render
 * 1) An annotation value on the label (in the given language), if available otherwise
 * 2) A property value on the label property (again, given the language), otherwise
 * 3) uses the default shortFormProvider provided
 */
public class LabelShortFormProvider implements ShortFormProvider {

    private AnnotationValueShortFormProvider delegate;

    public LabelShortFormProvider(final OWLServer server, ShortFormProvider defaultSFP) {

        final ServerPropertiesAdapter<ServerProperty> properties = server.getProperties();

        final String lang = properties.get(ServerProperty.optionLabelLang);

        final OWLDataFactory df = server.getOWLOntologyManager().getOWLDataFactory();

        final OWLOntologySetProvider activeOntologiesSetProvider = new OWLOntologySetProvider() {
            public Set<OWLOntology> getOntologies() {
                return server.getActiveOntologies();
            }
        };
        
        // the property assertion sfp
        OWLDataProperty dataProp = df.getOWLDataProperty(IRI.create(properties.get(ServerProperty.optionLabelPropertyUri)));
        ShortFormProvider pValueProvider = new PropertyAssertionValueShortFormProvider(Collections.<OWLPropertyExpression<?,?>>singletonList(dataProp),
                                                                                       createLangMap((OWLDataPropertyExpression)dataProp, lang),
                                                                                       activeOntologiesSetProvider,
                                                                                       defaultSFP);

        // the annotation label sfp
        OWLAnnotationProperty annotProp = df.getOWLAnnotationProperty(IRI.create(properties.get(ServerProperty.optionLabelUri)));
        delegate = new AnnotationValueShortFormProvider(Collections.singletonList(annotProp),
                                                        createLangMap(annotProp, lang),
                                                        activeOntologiesSetProvider,
                                                        pValueProvider);
    }

    public String getShortForm(OWLEntity owlEntity) {
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
