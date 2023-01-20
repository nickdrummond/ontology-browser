package org.coode.owl.mngr.impl;

import com.google.common.collect.Sets;
import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.OWLEntityFinder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.BidirectionalShortFormProvider;

import javax.annotation.Nonnull;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * Implementation of finder that takes a partial string and does a regexp search based on the renderings
 * in the name mapper provided.
 * - Stars (*) are replaced with .* for wildcard searching
 * - Searches are not case-sensitive
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

    public Set<OWLEntity> getOWLClasses (String str){
        return getMatches(str, NamedObjectType.classes);
    }

    public Set<OWLEntity> getOWLObjectProperties(String str) {
        return getMatches(str, NamedObjectType.objectproperties);
    }

    public Set<OWLEntity> getOWLDataProperties(String str) {
        return getMatches(str, NamedObjectType.dataproperties);
    }

    public Set<OWLEntity> getOWLAnnotationProperties(String str) {
        return getMatches(str, NamedObjectType.annotationproperties);
    }

    public Set<OWLEntity> getOWLIndividuals(String str) {
        return getMatches(str, NamedObjectType.individuals);
    }

    public Set<OWLEntity> getOWLDatatypes(String str) {
        return getMatches(str, NamedObjectType.datatypes);
    }

    public Set<OWLEntity> getOWLProperties(String str) {
        Set<OWLEntity> results = Sets.newHashSet();
        results.addAll(getOWLObjectProperties(str));
        results.addAll(getOWLDataProperties(str));
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str) {
        Set<OWLEntity> results = Sets.newHashSet();
        for (NamedObjectType subType : NamedObjectType.entitySubtypes()){
            results.addAll(getMatches(str, subType));
        }
        return results;
    }

    public Set<OWLEntity> getOWLEntities(String str, NamedObjectType type) {
        return getMatches(str, type);
    }

    public Set<OWLEntity> getOWLEntities(String str, NamedObjectType type, OWLOntology ont) {
        final Set<OWLEntity> results = getOWLEntities(str, type);
        if (ont != null){
            results.removeIf(result -> !ont.containsEntityInSignature(result.getIRI()));
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
            results.removeIf(result -> !ont.containsEntityInSignature(result));
        }
        return results;
    }

    public void dispose() {
        cache.dispose();
    }

    private Set<OWLEntity> getMatches(@Nonnull String str,
                                      @Nonnull NamedObjectType type){

        HashSet<OWLEntity> results = Sets.newHashSet();

        try{
            Pattern pattern = Pattern.compile(str.toLowerCase());

            Class<? extends OWLObject> typeClass = type.getCls();

            // not very efficient looking at all entity types
            for (String rendering : cache.getShortForms()) {
                Matcher m = pattern.matcher(rendering.toLowerCase());
                if (m.matches()) {
                    for (OWLEntity entity : cache.getEntities(rendering)){
                        if (typeClass.isAssignableFrom(entity.getClass())){
                            results.add(entity);
                        }
                    }
                }
            }
        }
        catch(PatternSyntaxException e){
            e.printStackTrace();
        }

        return results;
    }

    private Iterable<? extends OWLOntology> getActiveOntologies() {
        return activeOntologyProvider.getActiveOntology().getImportsClosure();
    }
}
