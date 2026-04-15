package org.ontbrowser.www.backend.db;

import com.google.common.collect.Sets;
import org.ontbrowser.www.backend.OWLEntityFinder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import owlapi.DBLabelSearch;

import java.util.List;
import java.util.Set;

public class DBEntityFinder implements OWLEntityFinder {

    private static final int MAX_SEARCH_RESULTS = 30;
    private final ShortFormProvider sfp;
    private final OWLDataFactory df;
    private final DBLabelSearch dbLabelSearch;

    public DBEntityFinder(ShortFormProvider sfp, OWLDataFactory df, DBLabelSearch dbLabelSearch) {
        this.sfp = sfp;
        this.df = df;
        this.dbLabelSearch = dbLabelSearch;
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        return sfp;
    }

    @Override
    public List<OWLClass> getOWLClasses(String str) {
        return dbLabelSearch.search(str, EntityType.CLASS, MAX_SEARCH_RESULTS);
    }

    @Override
    public List<OWLObjectProperty> getOWLObjectProperties(String str) {
        return getOWLObjectProperties(str, MAX_SEARCH_RESULTS);
    }

    @Override
    public List<OWLObjectProperty> getOWLObjectProperties(String str, int limit) {
        return dbLabelSearch.search(str, EntityType.OBJECT_PROPERTY, limit);
    }

    @Override
    public List<OWLDataProperty> getOWLDataProperties(String str) {
        return getOWLDataProperties(str, MAX_SEARCH_RESULTS);
    }

    @Override
    public List<OWLDataProperty> getOWLDataProperties(String str, int limit) {
        return dbLabelSearch.search(str, EntityType.DATA_PROPERTY, limit);
    }

    @Override
    public List<OWLAnnotationProperty> getOWLAnnotationProperties(String str) {
        return dbLabelSearch.search(str, EntityType.ANNOTATION_PROPERTY, MAX_SEARCH_RESULTS);
    }

    @Override
    public List<OWLNamedIndividual> getOWLIndividuals(String str) {
        return getOWLIndividuals(str, MAX_SEARCH_RESULTS);
    }

    @Override
    public List<OWLNamedIndividual> getOWLIndividuals(String str, int limit) {
        return dbLabelSearch.search(str, EntityType.NAMED_INDIVIDUAL, limit);
    }

    @Override
    public List<OWLDatatype> getOWLDatatypes(String str) {
        return dbLabelSearch.search(str, EntityType.DATATYPE, MAX_SEARCH_RESULTS);
    }

    @Override
    public List<OWLEntity> getOWLEntities(String str) {
        return dbLabelSearch.search(str, MAX_SEARCH_RESULTS);
    }

    // TODO this feels like a different lookup (IRI)

    @Override
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
}
