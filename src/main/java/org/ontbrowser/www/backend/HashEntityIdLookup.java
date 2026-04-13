package org.ontbrowser.www.backend;

import org.ontbrowser.www.exception.EntityNotFoundException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.BidirectionalShortFormProviderAdapter;

import java.util.HashMap;
import java.util.Map;

/**
 * ID cache for OWL entities. ID is the hash code of the IRI.
 */
public class HashEntityIdLookup implements EntityIdLookup {

    // for backwards compatibility
    private final Map<OWLOntologyID, BidirectionalShortFormProviderAdapter> caches = new HashMap<>();

    public HashEntityIdLookup(OWLOntologyManager mngr) {
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

    @Override
    public <E extends OWLEntity> E entityFor(String id, OWLOntology ont, Class<E> clazz) {
        var sfp = caches.get(ont.getOntologyID());
            return sfp.getEntities(id).stream()
                    .filter(clazz::isInstance)
                    .findFirst()
                    .map(clazz::cast)
                    .orElseThrow(() -> new EntityNotFoundException(id, ont, clazz));
    }
}
