package org.ontbrowser.www.renderer;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.util.IRIShortFormProvider;

import java.util.Map;

public class PrefixIRIShortFormProvider implements IRIShortFormProvider {

    private final Map<String, String> prefixMap;

    public PrefixIRIShortFormProvider(Map<String, String> prefixMap) {
        this.prefixMap = prefixMap;
    }

    @Override
    public String getShortForm(final IRI iri) {
        var iriString = iri.toString();
        for (var prefix2IRI : prefixMap.entrySet()) {
            String namespace = prefix2IRI.getValue();
            if (iriString.startsWith(namespace)) {
                String shortForm = iriString.substring(namespace.length());
                return prefix2IRI.getKey() + shortForm;
            }
        }
        String fragment = iri.getFragment();
        if (fragment != null) {
            return fragment;
        }
        return iriString;
    }
}
