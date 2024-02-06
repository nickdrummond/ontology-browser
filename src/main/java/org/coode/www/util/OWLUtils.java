package org.coode.www.util;

import org.semanticweb.owlapi.model.OWLDeclarationAxiom;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

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
}
