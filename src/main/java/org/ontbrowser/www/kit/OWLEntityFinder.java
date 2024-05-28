package org.ontbrowser.www.kit;

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

    Set<OWLEntity> getOWLEntities(String str, OWLOntology ont);

    <T extends OWLEntity> Set<T> getOWLEntities(String str, EntityType<T> type, OWLOntology ont);

    Set<OWLEntity> getOWLEntities(IRI iri, OWLOntology ont);

    void dispose();
}
