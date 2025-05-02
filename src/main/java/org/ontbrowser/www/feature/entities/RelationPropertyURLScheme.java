package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.url.RestURLScheme;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class RelationPropertyURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations";

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLObjectProperty objProp) {
            return ROOT_PATH
                    + "/onproperty/" + getIdForEntity(objProp);
        }
        return super.getURLForOWLObject(owlObject);
    }
}
