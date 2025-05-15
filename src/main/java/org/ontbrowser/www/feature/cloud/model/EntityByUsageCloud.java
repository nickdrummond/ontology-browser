package org.ontbrowser.www.feature.cloud.model;

import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.Set;

class EntityByUsageCloud<T extends OWLEntity> extends AbstractCloudModel<T>{

    private final Set<T> entities;
    private final Imports imports;
    private final OWLOntology ont;

    EntityByUsageCloud(OWLOntology ont, Set<T> entities, Imports imports) {
        this.ont = ont;
        this.entities = entities;
        this.imports = imports;
    }

    protected int calculateValue(T entity) {
        return (int) ont.referencingAxioms(entity, imports).count();
    }

    @Override
    public Set<T> getEntities() {
        return entities;
    }
}