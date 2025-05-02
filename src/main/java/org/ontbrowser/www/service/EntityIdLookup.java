package org.ontbrowser.www.service;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.BidirectionalShortFormProviderAdapter;
import org.springframework.web.server.ResponseStatusException;

import java.util.HashMap;
import java.util.Map;

import static org.springframework.http.HttpStatus.NOT_FOUND;

/**
 * ID cache for OWL entities. ID is the hash code of the IRI.
 */
public class EntityIdLookup {

    private final Map<OWLOntologyID, BidirectionalShortFormProviderAdapter> caches = new HashMap<>();

    public EntityIdLookup(OWLOntologyManager mngr) {
        mngr.ontologies().forEach(ontology ->
                caches.put(ontology.getOntologyID(), makeCache(ontology)));
    }

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

    public String getId(OWLEntity entity) {
        return String.valueOf(entity.getIRI().hashCode());
    }

    public <E extends OWLEntity> E entityFor(String id, OWLOntology ont, Class<E> clazz) {
        var sfp = caches.get(ont.getOntologyID());
        return sfp.getEntities(id).stream()
                .filter(clazz::isInstance)
                .findFirst()
                .map(clazz::cast)
                .orElseThrow(() -> new ResponseStatusException(NOT_FOUND, clazz.getSimpleName() + " not found: " + id));
    }
}
