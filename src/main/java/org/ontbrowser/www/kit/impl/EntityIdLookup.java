package org.ontbrowser.www.kit.impl;

import org.ontbrowser.www.exception.EntityNotFoundException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.BidirectionalShortFormProviderAdapter;

import java.util.HashMap;
import java.util.Map;

/**
 * ID cache for OWL entities. ID is the hash code of the IRI.
 */
public class EntityIdLookup {

    private final Map<String, String> prefixMap;

    // for backwards compatibility
    private final Map<OWLOntologyID, BidirectionalShortFormProviderAdapter> caches = new HashMap<>();

    public EntityIdLookup(OWLOntologyManager mngr, Map<String, String> prefixMap) {
        this.prefixMap = prefixMap;
        mngr.ontologies().forEach(ontology ->
                caches.put(ontology.getOntologyID(), makeCache(ontology)));
    }

    // legacy
    private BidirectionalShortFormProviderAdapter makeCache(OWLOntology ontology) {
        var cache = new BidirectionalShortFormProviderAdapter(ontology.getImportsClosure(), this::getId);
        var df = ontology.getOWLOntologyManager().getOWLDataFactory();
        cache.add(df.getOWLThing());
        cache.add(df.getOWLNothing());
        cache.add(df.getOWLTopObjectProperty());
        cache.add(df.getOWLTopDataProperty());
        cache.add(df.getTopDatatype());
        return cache;
    }

    // legacy
    private String getId(OWLEntity entity) {
        return String.valueOf(entity.getIRI().hashCode());
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
        else { // legacy
            var sfp = caches.get(ont.getOntologyID());
            return sfp.getEntities(id).stream()
                    .filter(clazz::isInstance)
                    .findFirst()
                    .map(clazz::cast)
                    .orElseThrow(() -> new EntityNotFoundException(id, ont, clazz));
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
