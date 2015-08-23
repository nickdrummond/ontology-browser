package org.coode.owl.mngr.impl;

import com.google.common.collect.Sets;
import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLEntityFinder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.BidirectionalShortFormProvider;

import java.util.Collections;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Implementation of finder that takes a partial string and does a regexp search based on the renderings
 * in the name mapper provided.
 * - Stars (*) are replaced with .* for wildcard searching
 * - Searches are not case sensitive
 * - Search string must be valid java regexp (apart from variations described above)
 */
public class OWLEntityFinderImpl implements OWLEntityFinder {

    private final BidirectionalShortFormProvider cache;

    private final OWLDataFactory owlDataFactory;

    private final ActiveOntologyProvider activeOntologyProvider;

    public OWLEntityFinderImpl(BidirectionalShortFormProvider cache,
                               OWLDataFactory owlDataFactory,
                               ActiveOntologyProvider activeOntologyProvider) {
        this.cache = cache;
        this.owlDataFactory = owlDataFactory;
        this.activeOntologyProvider = activeOntologyProvider;
    }

    public Set<OWLClass> getOWLClasses (String str){
        Set<OWLClass> results = Sets.newHashSet();
        getMatches(str, results, NamedObjectType.classes);
        return results;
    }

    public Set<OWLObjectProperty> getOWLObjectProperties(String str) {
        Set<OWLObjectProperty> results = Sets.newHashSet();
        getMatches(str, results, NamedObjectType.objectproperties);
        return results;
    }

    public Set<OWLDataProperty> getOWLDataProperties(String str) {
        Set<OWLDataProperty> results = Sets.newHashSet();
        getMatches(str, results, NamedObjectType.dataproperties);
        return results;
    }


    public Set<OWLAnnotationProperty> getOWLAnnotationProperties(String str) {
        Set<OWLAnnotationProperty> results = Sets.newHashSet();
        getMatches(str, results, NamedObjectType.annotationproperties);
        return results;
    }


    public Set<OWLNamedIndividual> getOWLIndividuals(String str) {
        Set<OWLNamedIndividual> results = Sets.newHashSet();
        getMatches(str, results, NamedObjectType.individuals);
        return results;
    }


    public Set<OWLDatatype> getOWLDatatypes(String str) {
        Set<OWLDatatype> results = Sets.newHashSet();
        getMatches(str, results, NamedObjectType.datatypes);
        return results;
    }


    public Set<OWLProperty> getOWLProperties(String str) {
        Set<OWLProperty> results = Sets.newHashSet();
        results.addAll(getOWLObjectProperties(str));
        results.addAll(getOWLDataProperties(str));
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str) {
        Set<OWLEntity> results = Sets.newHashSet();
        for (NamedObjectType subType : NamedObjectType.entitySubtypes()){
            getMatches(str, results, subType);
        }
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str, NamedObjectType type) {
        Set<OWLEntity> results = Sets.newHashSet();
        getMatches(str, results, type);
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str, NamedObjectType type, OWLOntology ont) {
        final Set<OWLEntity> results = getOWLEntities(str, type);
        if (ont != null){
            for (OWLEntity result : results){
                if (!ont.containsEntityInSignature(result.getIRI())){
                    results.remove(result);
                }
            }
        }
        return results;
    }

    public Set<OWLEntity> getOWLEntities(IRI iri, NamedObjectType type) {
        if (!iri.isAbsolute()){
            throw new IllegalArgumentException("URI must be absolute");
        }

        switch(type){
            case entities:
                Set<OWLEntity> results = Sets.newHashSet();
                for (NamedObjectType subType : NamedObjectType.entitySubtypes()){
                    OWLEntity entity = subType.getOWLEntity(iri, owlDataFactory);
                    for (OWLOntology ont : getActiveOntologies()){
                        if (ont.containsEntityInSignature(entity)){
                            results.add(entity);
                        }
                    }
                }
                return results;
            case ontologies:
                throw new RuntimeException("Cannot get entities of type ontology");
            default:
                OWLEntity entity = type.getOWLEntity(iri, owlDataFactory);
                for (OWLOntology ont : getActiveOntologies()){
                    if (ont.containsEntityInSignature(entity)){
                        return Collections.singleton(entity);
                    }
                }
        }

        return Collections.emptySet();
    }

    public Set<OWLEntity> getOWLEntities(IRI iri, NamedObjectType type, OWLOntology ont) {
        final Set<OWLEntity> results = getOWLEntities(iri, type);
        if (ont != null){
            for (OWLEntity result : results){
                if (!ont.containsEntityInSignature(result)){
                    results.remove(result);
                }
            }
        }
        return results;
    }

    public void dispose() {
        cache.dispose();
    }

    private <T extends OWLEntity> void getMatches(String str, Set<T> results, NamedObjectType type){
        try{
            Pattern pattern = Pattern.compile(str.toLowerCase());
            for (String rendering : cache.getShortForms()) { // not very efficient looking at all entity types
                Matcher m = pattern.matcher(rendering.toLowerCase());
                if (m.matches()) {
                    for (OWLEntity entity : cache.getEntities(rendering)){
                        if (type.getCls().isAssignableFrom(entity.getClass())){
                            results.add((T)entity);
                        }
                    }
                }
            }
        }
        catch(PatternSyntaxException e){
            e.printStackTrace();
        }
    }

    private Iterable<? extends OWLOntology> getActiveOntologies() {
        return activeOntologyProvider.getActiveOntology().getImportsClosure();
    }
}
