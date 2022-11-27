package org.coode.www.renderer;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.HashSet;
import java.util.Set;

public class QuotingBiDirectionalShortFormProvider extends CachingBidirectionalShortFormProvider {
    private final ShortFormProvider provider;

    public QuotingBiDirectionalShortFormProvider(ShortFormProvider provider, Set<OWLOntology> ontologies) {
        super();
        this.provider = provider;
        // TODO: should names also include all standard xsd datatypes - not just the ones referenced?
        Set<OWLEntity> entities = new HashSet<>();
        for (OWLOntology ont: ontologies) {
            entities.addAll(ont.getSignature());
        };
        rebuild(entities.stream());
    }

    protected String generateShortForm(OWLEntity owlEntity) {
            String shortform = provider.getShortForm(owlEntity);
            if (shortform.indexOf(" ") > -1){ // if this is a multiword name
                shortform = "\"" + shortform + "\"";
            }
            return shortform;
        }
}
