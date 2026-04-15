package org.ontbrowser.www.backend.memory.kit.impl;

import com.google.common.collect.Sets;
import org.ontbrowser.www.backend.OWLEntityFinder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.BidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import javax.annotation.Nonnull;
import java.util.*;
import java.util.regex.Pattern;

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

    public OWLEntityFinderImpl(BidirectionalShortFormProvider cache,
                               OWLDataFactory df) {
        this.cache = cache;
        this.df = df;
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        return cache;
    }

    public List<OWLClass> getOWLClasses (String str){
        return getMatches(str, EntityType.CLASS);
    }

    public List<OWLObjectProperty> getOWLObjectProperties(String str) {
        return getMatches(str, EntityType.OBJECT_PROPERTY);
    }

    public List<OWLObjectProperty> getOWLObjectProperties(String str, int limit) {
        return getMatches(str, EntityType.OBJECT_PROPERTY, limit);
    }

    public List<OWLDataProperty> getOWLDataProperties(String str) {
        return getMatches(str, EntityType.DATA_PROPERTY);
    }

    public List<OWLDataProperty> getOWLDataProperties(String str, int limit) {
        return getMatches(str, EntityType.DATA_PROPERTY, limit);
    }

    public List<OWLAnnotationProperty> getOWLAnnotationProperties(String str) {
        return getMatches(str, EntityType.ANNOTATION_PROPERTY);
    }

    public List<OWLNamedIndividual> getOWLIndividuals(String str) {
        return getMatches(str, EntityType.NAMED_INDIVIDUAL);
    }

    public List<OWLNamedIndividual> getOWLIndividuals(String str, int limit) {
        return getMatches(str, EntityType.NAMED_INDIVIDUAL, limit);
    }

    public List<OWLDatatype> getOWLDatatypes(String str) {
        return getMatches(str, EntityType.DATATYPE);
    }

    public List<OWLEntity> getOWLEntities(String str) {
        Pattern pattern = Pattern.compile(str.toLowerCase());

        // not very efficient looking at all entity types
        return cache.getShortForms().stream()
                .filter(sf -> pattern.matcher(sf.toLowerCase()).matches())
                .map(cache::getEntities)
                .flatMap(Collection::stream)
                .toList();
    }

    public Set<OWLEntity> getOWLEntities(IRI iri, OWLOntology ont) {
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

    private <T extends OWLEntity> List<T> getMatches(
            @Nonnull String str,
            @Nonnull EntityType<T> type){
        return getOWLEntities(str).stream()
                .filter(e -> type.equals(e.getEntityType()))
                .map(e -> correctType(e, type))
                .toList();
    }

    private <T extends OWLEntity> List<T> getMatches(
            @Nonnull String str,
            @Nonnull EntityType<T> type,
            int limit
    ){
        return getOWLEntities(str).stream()
                .filter(e -> type.equals(e.getEntityType()))
                .map(e -> correctType(e, type))
                .limit(limit)
                .toList();
    }

    private <T extends OWLEntity> T correctType(OWLEntity e, EntityType<T> type) {
        if (!e.getEntityType().equals(type)) {
            throw new RuntimeException(e + " is not of type " + type);
        }
        return type.buildEntity(e.getIRI(), df);
    }
}
