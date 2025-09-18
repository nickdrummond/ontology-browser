package org.ontbrowser.www.util;

import org.semanticweb.owlapi.model.*;
import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

public class OWLUtils {

    private OWLUtils() {}

    public static OWLOntology getDeclaringOntology(OWLEntity e, OWLOntology rootOnt) {
        OWLDeclarationAxiom decl = rootOnt.getOWLOntologyManager().getOWLDataFactory().getOWLDeclarationAxiom(e);
        for (OWLOntology o : rootOnt.getImportsClosure()) {
            if (o.containsAxiom(decl)) {
                return o;
            }
        }
        return rootOnt;
    }

    public static String ontIRI(OWLOntology ont) {
        return ontIRI(ont.getOntologyID());
    }

    public static String ontIRI(OWLOntologyID ontologyID) {
        return ontologyID.getOntologyIRI().map(IRI::toString)
                .orElse(ontologyID.getDefaultDocumentIRI().map(IRI::toString)
                        .orElse("anonymous"));
    }

    public static String shortOntName(OWLOntologyID ontologyID) {
        return ontologyID.getOntologyIRI().map(OWLUtils::shortName)
                .orElse(ontologyID.getDefaultDocumentIRI().map(OWLUtils::shortName)
                        .orElse("anonymous"));
    }

    public static String shortName(IRI iri) {
        String s = iri.toString();
        int pos = Math.max(s.lastIndexOf('#'), s.lastIndexOf('/'));
        if (pos != -1 && pos < s.length() - 1) {
            return s.substring(pos + 1);
        }
        return s;
    }

    public static EntityType<?> getEntityTypeFromPath(String name) {
        return switch(name) {
            case "classes" -> EntityType.CLASS;
            case "individuals" -> EntityType.NAMED_INDIVIDUAL;
            case "objectproperties" -> EntityType.OBJECT_PROPERTY;
            case "dataproperties" -> EntityType.DATA_PROPERTY;
            case "annotationproperties" -> EntityType.ANNOTATION_PROPERTY;
            case "datatypes" -> EntityType.DATATYPE;
            default -> throw new ResponseStatusException(HttpStatus.NOT_FOUND, "Entity type not found: " + name);
        };
    }
}
