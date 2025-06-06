package org.ontbrowser.www.renderer;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

/**
 * SimpleShortFormProvider does not manage names for IRIs ending in "/"
 * Or fragments beginning with numbers
 */
public class FixedSimpleShortFormProvider implements ShortFormProvider {

    private final IRIShortFormProvider iriShortFormProvider;

    public FixedSimpleShortFormProvider(IRIShortFormProvider iriShortFormProvider) {
        this.iriShortFormProvider = iriShortFormProvider;
    }

    @Override
    public String getShortForm(final OWLEntity entity) {
        String iriString = entity.getIRI().toString();
        if (iriString.endsWith("#") || iriString.endsWith("/")) {
            return iriShortFormProvider.getShortForm(IRI.create(iriString.substring(0, iriString.length()-1)));
        }
        return iriShortFormProvider.getShortForm(entity.getIRI());
    }
}
