package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class RelationPropertyURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations";
    private boolean isInverse = false;

    public RelationPropertyURLScheme(OWLHTMLKit kit) {
        super(kit);
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLObjectProperty) {
            return ROOT_PATH
                    + "/onproperty/" + getIdForEntity((OWLObjectProperty) owlObject)
                    + "/?inverse=" + isInverse;
        }
        return super.getURLForOWLObject(owlObject);
    }
}
