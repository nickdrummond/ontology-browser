package org.coode.www.renderer;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.util.SimpleIRIShortFormProvider;

/**
 * SimpleShortFormProvider does not manage names for IRIs ending in "/"
 */
public class FixedSimpleShortFormProvider implements ShortFormProvider {

    private SimpleIRIShortFormProvider iriShortFormProvider = new SimpleIRIShortFormProvider();

    @Override
    public String getShortForm(final OWLEntity entity) {
        String iriString = entity.getIRI().toString();
        if (iriString.endsWith("/")) {
            return iriShortFormProvider.getShortForm(IRI.create(iriString.substring(0, iriString.length()-1)));
        }
        return iriShortFormProvider.getShortForm(entity.getIRI());
    }

    @Override
    public void dispose() {
    }
}
