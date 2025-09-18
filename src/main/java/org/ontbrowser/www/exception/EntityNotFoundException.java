package org.ontbrowser.www.exception;

import org.ontbrowser.www.util.OWLUtils;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

public class EntityNotFoundException extends RuntimeException {
    private final OWLOntology ontology;

    public EntityNotFoundException(String id, OWLOntology ontology, Class<? extends OWLEntity> clazz) {
        super(clazz.getSimpleName() + " not found: " + id + " in ontology " + OWLUtils.shortOntName(ontology.getOntologyID()));
        this.ontology = ontology;
    }

    public OWLOntology getOntology() {
        return ontology;
    }
}