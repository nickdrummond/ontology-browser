package org.coode.www.renderer;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.ReferencedEntitySetProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Set;

public class QuotingBiDirectionalShortFormProvider extends CachingBidirectionalShortFormProvider {
    private final ShortFormProvider provider;

    public QuotingBiDirectionalShortFormProvider(ShortFormProvider provider, Set<OWLOntology> ontologies) {
        super();
        this.provider = provider;
        // TODO: should names also include all standard xsd datatypes - not just the ones referenced?
        rebuild(new ReferencedEntitySetProvider(ontologies));
    }

    protected String generateShortForm(OWLEntity owlEntity) {
            String shortform = provider.getShortForm(owlEntity);
            if (shortform.indexOf(" ") > -1){ // if this is a multiword name
                shortform = "\"" + shortform + "\"";
            }
            return shortform;
        }
}
