package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.url.RestURLScheme;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class GraphURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/graph";

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        return ROOT_PATH + super.getURLForOWLObject(owlObject);
    }
}
