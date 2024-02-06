package org.coode.www.kit.impl;

import com.google.common.collect.Sets;
import org.coode.www.kit.ActiveOntologyProvider;
import org.coode.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.BidirectionalShortFormProvider;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Implementation of finder that takes a partial string and does a regexp search based on the renderings
 * in the name mapper provided.
 * - Stars (*) are replaced with .* for wildcard searching
 * - Searches are not case-sensitive
 * - Search string must be valid java regexp (apart from variations described above)
 */
public class OWLEntityFinderImpl implements OWLEntityFinder {

    private final BidirectionalShortFormProvider cache;

    private final OWLDataFactory df;

    private final ActiveOntologyProvider activeOntologyProvider;

    public OWLEntityFinderImpl(BidirectionalShortFormProvider cache,
                               OWLDataFactory df,
                               ActiveOntologyProvider activeOntologyProvider) {
        this.cache = cache;
        this.df = df;
        this.activeOntologyProvider = activeOntologyProvider;
    }

    public Set<OWLClass> getOWLClasses (String str){
        return getMatches(str, EntityType.CLASS);
    }

    public Set<OWLObjectProperty> getOWLObjectProperties(String str) {
        return getMatches(str, EntityType.OBJECT_PROPERTY);
    }

    public Set<OWLDataProperty> getOWLDataProperties(String str) {
        return getMatches(str, EntityType.DATA_PROPERTY);
    }

    public Set<OWLAnnotationProperty> getOWLAnnotationProperties(String str) {
        return getMatches(str, EntityType.ANNOTATION_PROPERTY);
    }

    public Set<OWLNamedIndividual> getOWLIndividuals(String str) {
        return getMatches(str, EntityType.NAMED_INDIVIDUAL);
    }

    public Set<OWLDatatype> getOWLDatatypes(String str) {
        return getMatches(str, EntityType.DATATYPE);
    }

    public Set<OWLEntity> getOWLProperties(String str) {
        Set<OWLEntity> results = Sets.newHashSet();
        results.addAll(getOWLObjectProperties(str));
        results.addAll(getOWLDataProperties(str));
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str) {
        Pattern pattern = Pattern.compile(str.toLowerCase());

        // not very efficient looking at all entity types
        return cache.getShortForms().stream()
                .filter(sf -> pattern.matcher(sf.toLowerCase()).matches())
                .map(cache::getEntities)
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());
    }

    public <T extends OWLEntity> Set<T> getOWLEntities(String str, EntityType<T> type) {
        return getMatches(str, type);
    }

    public <T extends OWLEntity> Set<T> getOWLEntities(String str, EntityType<T> type, OWLOntology ont) {
        final Set<T> results = getOWLEntities(str, type);
        if (ont != null){
            results.removeIf(result -> !ont.containsEntityInSignature(result.getIRI()));
        }
        return results;
    }

    public Set<OWLEntity> getOWLEntities(IRI iri) {
        OWLOntology ont = activeOntologyProvider.getActiveOntology();
        return getOWLEntities(iri, ont, Imports.INCLUDED);
    }

    public Set<OWLEntity> getOWLEntities(IRI iri, OWLOntology ont, Imports imports) {
        Set<OWLEntity> results = Sets.newHashSet();
        if (ont.containsClassInSignature(iri, imports)) {
            results.add(df.getOWLClass(iri));
        }
        if (ont.containsObjectPropertyInSignature(iri, imports)) {
            results.add(df.getOWLObjectProperty(iri));
        }
        if (ont.containsDataPropertyInSignature(iri, imports)) {
            results.add(df.getOWLDataProperty(iri));
        }
        if (ont.containsAnnotationPropertyInSignature(iri, imports)) {
            results.add(df.getOWLAnnotationProperty(iri));
        }
        if (ont.containsIndividualInSignature(iri, imports)) {
            results.add(df.getOWLNamedIndividual(iri));
        }
        if (ont.containsDatatypeInSignature(iri, imports)) {
            results.add(df.getOWLDatatype(iri));
        }
        return results;
    }

    public <T extends OWLEntity> Optional<T> getOWLEntity(IRI iri, EntityType<T> type) {
        return getOWLEntity(iri, type, activeOntologyProvider.getActiveOntology(), Imports.INCLUDED);
    }

    public <T extends OWLEntity> Optional<T> getOWLEntity(IRI iri, EntityType<T> type, OWLOntology ont) {
        return getOWLEntity(iri, type, ont, Imports.INCLUDED); // INCLUDED OR NOT?
    }

    public <T extends OWLEntity> Optional<T> getOWLEntity(IRI iri, EntityType<T> type, OWLOntology ont, Imports imports) {
        boolean found =
                (type.equals(EntityType.CLASS) && (ont.containsClassInSignature(iri, imports))) ||
                (type.equals(EntityType.OBJECT_PROPERTY) && (ont.containsObjectPropertyInSignature(iri, imports))) ||
                (type.equals(EntityType.DATA_PROPERTY) && (ont.containsDataPropertyInSignature(iri, imports))) ||
                (type.equals(EntityType.ANNOTATION_PROPERTY) && (ont.containsAnnotationPropertyInSignature(iri, imports))) ||
                (type.equals(EntityType.NAMED_INDIVIDUAL) && (ont.containsIndividualInSignature(iri, imports))) ||
                (type.equals(EntityType.DATATYPE) && (ont.containsDatatypeInSignature(iri, imports)));

        return found ? Optional.of(type.buildEntity(iri, df)) : Optional.empty();
    }

    public void dispose() {
        cache.dispose();
    }

    private <T extends OWLEntity> Set<T> getMatches(
            @Nonnull String str,
            @Nonnull EntityType<T> type){
        return getOWLEntities(str).stream()
                .filter(e -> type.equals(e.getEntityType()))
                .map(e -> correctType(e, type))
                .collect(Collectors.toSet());
    }

    private <T extends OWLEntity> T correctType(OWLEntity e, EntityType<T> type) {
        if (!e.getEntityType().equals(type)) {
            throw new RuntimeException(e + " is not of type " + type);
        }
        return type.buildEntity(e.getIRI(), df);
    }
}
