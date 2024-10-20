package org.ontbrowser.www.model.graph;

import org.semanticweb.owlapi.model.*;

// cannot use classes as nodes as this may be repeated elsewhere (and the nodes get merged)
public class ProxyBuilder {

    public static final String SUFFIX = "----anon";
    private final OWLDataFactory df;
    private int anonInc = 0;

    public ProxyBuilder(OWLDataFactory df) {
        this.df = df;
    }

    public OWLNamedIndividual createAnonNode(OWLClass cls) {
        return df.getOWLNamedIndividual(cls.getIRI().toString() + SUFFIX + ++anonInc);
    }

    public OWLEntity getProxyOrOriginal(OWLEntity object) {
        if (object instanceof OWLNamedIndividual ind) {
            var iri = ind.getIRI().toString();
            var suffix = iri.indexOf(SUFFIX);
            if (suffix > 0) {
                var clsIRI = iri.substring(0, suffix);
                return df.getOWLClass(clsIRI);
            }
        }
        return object;
    }


}
