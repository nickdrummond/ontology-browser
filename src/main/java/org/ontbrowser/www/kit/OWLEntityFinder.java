package org.ontbrowser.www.kit;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.List;
import java.util.Set;

/**
 * Interface describing string -> named object behaviour.
 * Up to the implementation whether this is an absolute match based on eg:
 * - string version of URI
 * - string rendering of human-readable name
 * - partial string match (for search)
 */
public interface OWLEntityFinder {

    ShortFormProvider getShortFormProvider();

    List<OWLClass> getOWLClasses(String str);

    List<OWLObjectProperty> getOWLObjectProperties(String str);
    List<OWLObjectProperty> getOWLObjectProperties(String str, int limit);

    List<OWLDataProperty> getOWLDataProperties(String str);
    List<OWLDataProperty> getOWLDataProperties(String str, int limit);

    List<OWLAnnotationProperty> getOWLAnnotationProperties(String str);

    List<OWLNamedIndividual> getOWLIndividuals(String str);
    List<OWLNamedIndividual> getOWLIndividuals(String str, int limit);

    List<OWLDatatype> getOWLDatatypes(String str);

    List<OWLEntity> getOWLEntities(String str);

    List<OWLEntity> getOWLEntities(String str, OWLOntology ont);

    <T extends OWLEntity> List<T> getOWLEntities(String str, EntityType<T> type, OWLOntology ont);

    Set<OWLEntity> getOWLEntities(IRI iri, OWLOntology ont);

    void dispose();
}
