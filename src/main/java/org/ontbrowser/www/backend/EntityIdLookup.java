package org.ontbrowser.www.backend;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

public interface EntityIdLookup {
    <E extends OWLEntity> E entityFor(String id, OWLOntology ont, Class<E> clazz);
}
