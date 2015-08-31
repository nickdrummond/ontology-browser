package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.*;

import java.util.Set;

/**
 * Interface describing string -> named object behaviour.
 * Up to the implementation whether this is an absolute match based on eg:
 * - string version of URI
 * - string rendering of human readable name
 * - partial string match (for search)
 */
public interface OWLEntityFinder {

    Set<OWLClass> getOWLClasses(String str);

    Set<OWLObjectProperty> getOWLObjectProperties(String str);

    Set<OWLDataProperty> getOWLDataProperties(String str);

    Set<OWLAnnotationProperty> getOWLAnnotationProperties(String str);

    Set<OWLNamedIndividual> getOWLIndividuals(String str);

    Set<OWLDatatype> getOWLDatatypes(String str);

    Set<OWLProperty> getOWLProperties(String str);

    Set<OWLEntity> getOWLEntities(String str);

    Set<? extends OWLEntity> getOWLEntities(String str, NamedObjectType type);
    Set<? extends OWLEntity> getOWLEntities(String str, NamedObjectType type, OWLOntology ont);

    /**
     *
     * @param iri
     *@param type  @return can return a set if the type is entities (an individual and a class could be returned)
     */
    Set<? extends OWLEntity> getOWLEntities(IRI iri, NamedObjectType type);
    Set<? extends OWLEntity> getOWLEntities(IRI iri, NamedObjectType type, OWLOntology ont);

    void dispose();
}
