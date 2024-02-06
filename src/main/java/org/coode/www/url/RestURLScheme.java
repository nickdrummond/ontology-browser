package org.coode.www.url;

import org.semanticweb.owlapi.model.*;

/**
 * A URL scheme for dynamic server-side resolution
 *
 * This will ONLY work if the hashCode() method for URIs is 1-1 mapping and completely deterministic
 * (for each vm the hash should be the same).
 * We may need to implement our own hash function if this is not the case.
 * It appears to work on OSX, but this will need to be double-checked.
 *
 * Creates URLs of the form:
 * serverbase/<type>/<hash-of-object-uri>/
 *
 */
public class RestURLScheme implements URLScheme {

    private OWLEntityVisitorEx<String> typeVisitor = new OWLEntityVisitorEx<>() {
        @Override
        public String visit(OWLClass ce) {
            return "classes";
        }

        @Override
        public String visit(OWLDatatype node) {
            return "datatypes";
        }

        @Override
        public String visit(OWLNamedIndividual individual) {
            return "individuals";
        }

        @Override
        public String visit(OWLObjectProperty property) {
            return "objectproperties";
        }

        @Override
        public String visit(OWLDataProperty property) {
            return "dataproperties";
        }

        @Override
        public String visit(OWLAnnotationProperty property) {
            return "annotationproperties";
        }
    };

    public String getURLForOWLObject(OWLObject owlObject) {
        if (owlObject == null){
            throw new NullPointerException("OWLObject may not be null");
        }

        String type;
        String code;
        StringBuilder sb = new StringBuilder("/"); // relative URLs

        if (owlObject instanceof OWLEntity){
            OWLEntity owlEntity = (OWLEntity) owlObject;
            type = getTypeForEntity(owlEntity);
            code = getIdForEntity(owlEntity);
        }
        else if (owlObject.isOntology()){
            type = "ontologies";
            code = String.valueOf(((OWLOntology)owlObject).getOntologyID().hashCode());
        }
        else{
            type = owlObject.getClass().getSimpleName();
            code = String.valueOf(owlObject.hashCode());
        }

        sb.append(type);
        sb.append("/");
        sb.append(code);
        sb.append("/");

        return sb.toString();
    }

    private String getTypeForEntity(OWLEntity owlEntity) {
        return owlEntity.accept(typeVisitor);
    }

    protected String getIdForEntity(OWLEntity entity) {
        return String.valueOf(entity.getIRI().hashCode());
    }
}
