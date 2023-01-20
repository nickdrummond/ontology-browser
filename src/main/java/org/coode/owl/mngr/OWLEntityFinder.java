package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.*;

import java.util.Set;

/**
 * Interface describing string -> named object behaviour.
 * Up to the implementation whether this is an absolute match based on eg:
 * - string version of URI
 * - string rendering of human-readable name
 * - partial string match (for search)
 */
public interface OWLEntityFinder {

    Set<OWLEntity> getOWLClasses(String str);

    Set<OWLEntity> getOWLObjectProperties(String str);

    Set<OWLEntity> getOWLDataProperties(String str);

    Set<OWLEntity> getOWLAnnotationProperties(String str);

    Set<OWLEntity> getOWLIndividuals(String str);

    Set<OWLEntity> getOWLDatatypes(String str);

    Set<OWLEntity> getOWLProperties(String str);

    Set<OWLEntity> getOWLEntities(String str);

    Set<? extends OWLEntity> getOWLEntities(String str, NamedObjectType type);
    Set<? extends OWLEntity> getOWLEntities(String str, NamedObjectType type, OWLOntology ont);

    /**
     * @param iri the id of the searched for entity
     * @param type the type of the searched for entity
     *
     * @return can return a set if the type is entities (an individual and a class could be returned)
     */
    Set<? extends OWLEntity> getOWLEntities(IRI iri, NamedObjectType type);
    Set<? extends OWLEntity> getOWLEntities(IRI iri, NamedObjectType type, OWLOntology ont);

    void dispose();
}
