package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectVisitorEx;
import org.semanticweb.owlapi.util.ShortFormProvider;

public class GraphURLScheme implements URLScheme {

    public static final String ROOT_PATH = "/graph";
    private final ShortFormProvider sfp;

    public GraphURLScheme(ShortFormProvider sfp) {
        this.sfp = sfp;
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        return owlObject.accept(new OWLObjectVisitorEx<String>() {
            @Override
            public String visit(OWLNamedIndividual individual) {
                return ROOT_PATH + "?indivs=" + sfp.getShortForm(individual);
            }
        });
    }
}
