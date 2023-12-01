package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.*;

import java.util.Optional;
import java.util.Set;

/**
 * Interface describing string -> named object behaviour.
 * Up to the implementation whether this is an absolute match based on eg:
 * - string version of URI
 * - string rendering of human-readable name
 * - partial string match (for search)
 */
public interface OWLEntityFinder {

    Set<OWLClass> getOWLClasses(String str);

    Set<OWLObjectProperty> getOWLObjectProperties(String str);

    Set<OWLDataProperty> getOWLDataProperties(String str);

    Set<OWLAnnotationProperty> getOWLAnnotationProperties(String str);

    Set<OWLNamedIndividual> getOWLIndividuals(String str);

    Set<OWLDatatype> getOWLDatatypes(String str);

    Set<OWLEntity> getOWLProperties(String str);

    Set<OWLEntity> getOWLEntities(String str);

    <T extends OWLEntity> Set<T> getOWLEntities(String str, EntityType<T> type);
    <T extends OWLEntity> Set<T> getOWLEntities(String str, EntityType<T> type, OWLOntology ont);

    Set<OWLEntity> getOWLEntities(IRI iri);

    /**
     * @param iri the id of the searched for entity
     * @param type the type of the searched for entity
     *
     * @return can return a set if the type is entities (an individual and a class could be returned)
     */
    <T extends OWLEntity> Optional<T> getOWLEntity(IRI iri, EntityType<T> type);
    <T extends OWLEntity> Optional<T> getOWLEntity(IRI iri, EntityType<T> type, OWLOntology ont);

    void dispose();
}
