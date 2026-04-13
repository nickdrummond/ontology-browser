package org.ontbrowser.www.backend;

import org.ontbrowser.www.exception.EntityNotFoundException;
import org.semanticweb.owlapi.model.*;

import java.util.Map;

public class QNameEntityIdLookup implements EntityIdLookup {

    private final Map<String, String> prefixMap;

    public QNameEntityIdLookup(Map<String, String> prefixMap) {
        this.prefixMap = prefixMap;
    }

    public <E extends OWLEntity> E entityFor(String id, OWLOntology ont, Class<E> clazz) {
        String[] parts = id.split(":");
        if (parts.length == 2) {
            String prefix = parts[0];
            String localName = parts[1];
            String base = prefixMap.get(prefix);
            if (base == null) {
                throw new EntityNotFoundException(id, ont, clazz);
            }
            String iriStr = base + localName;
            EntityType<? extends E> entityType = entityTypeForClass(clazz);
            return ont.getOWLOntologyManager().getOWLDataFactory().getOWLEntity(entityType, IRI.create(iriStr));
        }
        else {
            throw new EntityNotFoundException(id, ont, clazz);
        }
    }

    @SuppressWarnings("unchecked")
    private <E extends OWLEntity> EntityType<E> entityTypeForClass(Class<E> clazz) {
        if (OWLClass.class.isAssignableFrom(clazz))              return (EntityType<E>) EntityType.CLASS;
        if (OWLObjectProperty.class.isAssignableFrom(clazz))     return (EntityType<E>) EntityType.OBJECT_PROPERTY;
        if (OWLDataProperty.class.isAssignableFrom(clazz))       return (EntityType<E>) EntityType.DATA_PROPERTY;
        if (OWLAnnotationProperty.class.isAssignableFrom(clazz)) return (EntityType<E>) EntityType.ANNOTATION_PROPERTY;
        if (OWLNamedIndividual.class.isAssignableFrom(clazz))    return (EntityType<E>) EntityType.NAMED_INDIVIDUAL;
        if (OWLDatatype.class.isAssignableFrom(clazz))           return (EntityType<E>) EntityType.DATATYPE;
        throw new IllegalArgumentException("Unsupported entity type: " + clazz.getSimpleName());
    }
}
