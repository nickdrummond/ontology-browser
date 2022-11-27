package org.coode.www.renderer;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.util.SimpleIRIShortFormProvider;

/**
 * SimpleShortFormProvider does not manage names for IRIs ending in "/"
 * Or fragments beginning with numbers
 */
public class FixedSimpleShortFormProvider implements ShortFormProvider {

    private SimpleIRIShortFormProvider iriShortFormProvider = new SimpleIRIShortFormProvider();

    @Override
    public String getShortForm(final OWLEntity entity) {
        String iriString = entity.getIRI().toString();
        if (iriString.endsWith("#") || iriString.endsWith("/")) {
            return iriShortFormProvider.getShortForm(IRI.create(iriString.substring(0, iriString.length()-1)));
        }
        int hash = iriString.indexOf('#');
        if (hash >= 0 && hash < iriString.length()) {
            return iriString.substring(hash+1);
        }
        return iriShortFormProvider.getShortForm(entity.getIRI());
    }

    @Override
    public void dispose() {
    }
}
