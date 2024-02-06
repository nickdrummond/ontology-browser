package org.coode.www.url;

import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class RelationPropertyURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations";

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLObjectProperty) {
            return ROOT_PATH
                    + "/onproperty/" + getIdForEntity((OWLObjectProperty) owlObject)
                    + "/";
        }
        return super.getURLForOWLObject(owlObject);
    }
}
