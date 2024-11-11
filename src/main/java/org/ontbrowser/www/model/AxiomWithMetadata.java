package org.ontbrowser.www.model;

import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

public record AxiomWithMetadata(
        String type,
        OWLObject owlObject,
        OWLAxiom owlAxiom,
        OWLOntology owlOntology) {
}
