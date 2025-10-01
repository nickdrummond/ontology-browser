package org.ontbrowser.www.feature.entities;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Set;

public interface NamedTypeProvider<T extends OWLEntity> {
    Set<OWLEntity> getNamedTypes(T entity, OWLOntology ont);
}
