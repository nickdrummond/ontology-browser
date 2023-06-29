package org.coode.html.url;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;

public class RelationsURLScheme extends RestURLScheme {

    public static final String ROOT_PATH = "/relations/";
    private final boolean inverse;
    private final String propertyId;

    public RelationsURLScheme(OWLHTMLKit kit, String propertyId, boolean inverse) {
        super(kit);
        this.propertyId = propertyId;
        this.inverse = inverse;
    }

    @Override
    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject instanceof OWLObjectProperty) {
            return ROOT_PATH + getIdForEntity((OWLObjectProperty) owlObject) + "/?inverse=" + inverse;
        }
        if (owlObject instanceof OWLNamedIndividual) {
            return ROOT_PATH + propertyId + "/" + getIdForEntity((OWLNamedIndividual) owlObject) + "/?inverse=" + inverse;
        }
        return super.getURLForOWLObject(owlObject);
    }
}
