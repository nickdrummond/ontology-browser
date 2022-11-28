package org.coode.html.url;

import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

/**
 * URL scheme for dynamic server-side resolution
 *
 * This will ONLY work if the hashCode() method for URIs is 1-1 mapping and completely deterministic
 * (for each vm the hash should be the same).
 * We may need to implement our own hash function if this is not the case.
 * It appears to work on a mac, but this will need to be double checked.
 *
 * Creates URLs of the form:
 * serverbase/<type>/<hash-of-object-uri>/
 *
 */
public class RestURLScheme extends AbstractURLScheme {

    public RestURLScheme(OWLHTMLKit kit) {
        super(kit);
    }

    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject == null){
            throw new NullPointerException("OWLObject may not be null");
        }

        String type;
        int code;
        StringBuilder sb = new StringBuilder("/"); // relative URLs

        if (owlObject instanceof OWLEntity){
            type = NamedObjectType.getType(owlObject).toString();
            code = ((OWLEntity)owlObject).getIRI().hashCode();
        }
        else if (owlObject instanceof OWLOntology){
            type = NamedObjectType.getType(owlObject).toString();
            code = ((OWLOntology)owlObject).getOntologyID().hashCode();
        }
        else{
            type = owlObject.getClass().getSimpleName();
            code = owlObject.hashCode();
        }

        sb.append(type);
        sb.append("/");
        sb.append(code);
        sb.append("/");

        return sb.toString();
    }
}
