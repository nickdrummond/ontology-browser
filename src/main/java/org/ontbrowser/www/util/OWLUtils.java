package org.ontbrowser.www.util;

import org.semanticweb.owlapi.model.*;

public class OWLUtils {

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
}
