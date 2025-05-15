package org.ontbrowser.www.feature.cloud.model;

import org.semanticweb.owlapi.model.EntityType;
import org.semanticweb.owlapi.model.EntityTypeVisitorEx;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;

import java.util.Optional;
import java.util.Set;

public class CloudModelFactory {

    private CloudModelFactory() {
    }

    public static <T extends OWLEntity> CloudModel<T> getUsageCloud(
            EntityType<T> entityType,
            OWLOntology ont,
            Imports imports
    ) {
        Set<T> entities = getEntities(entityType, ont, imports);
        var cloud = new EntityByUsageCloud<>(ont, entities, imports);
        cloud.load();
        return cloud;
    }

    @SuppressWarnings("unchecked")
    private static <T extends OWLEntity> Set<T> getEntities(
            EntityType<T> entityType, OWLOntology ont, Imports imports) {
        return entityType.accept(new EntityTypeVisitorEx<Set<T>>() {
            @Override
            public Optional<Set<T>> visitCLASS() {
                return Optional.of((Set<T>)ont.getClassesInSignature(imports));
            }

            @Override
            public Optional<Set<T>> visitOBJECT_PROPERTY() {
                return Optional.of((Set<T>)ont.getObjectPropertiesInSignature(imports));
            }

            @Override
            public Optional<Set<T>> visitDATA_PROPERTY() {
                return Optional.of((Set<T>)ont.getDataPropertiesInSignature(imports));
            }

            @Override
            public Optional<Set<T>> visitANNOTATION_PROPERTY() {
                return Optional.of((Set<T>)ont.getAnnotationPropertiesInSignature(imports));
            }

            @Override
            public Optional<Set<T>> visitNAMED_INDIVIDUAL() {
                return Optional.of((Set<T>)ont.getIndividualsInSignature(imports));
            }

            @Override
            public Optional<Set<T>> visitDATATYPE() {
                return Optional.of((Set<T>)ont.getDatatypesInSignature(imports));
            }
        }).orElseThrow(() -> new RuntimeException("Unsupported entity type: " + entityType));    }
}
