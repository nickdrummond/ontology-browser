package org.ontbrowser.www.backend.db;

import com.google.common.collect.Sets;
import org.apache.jena.atlas.lib.NotImplemented;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Set;

public class DBEntityFinder implements OWLEntityFinder {

    private final ShortFormProvider sfp;
    private final OWLDataFactory df;

    public DBEntityFinder(ShortFormProvider sfp, OWLDataFactory df) {
        this.sfp = sfp;
        this.df = df;
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        return sfp;
    }

    @Override
    public Set<OWLClass> getOWLClasses(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLObjectProperty> getOWLObjectProperties(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLObjectProperty> getOWLObjectProperties(String str, int limit) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLDataProperty> getOWLDataProperties(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLDataProperty> getOWLDataProperties(String str, int limit) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLAnnotationProperty> getOWLAnnotationProperties(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLNamedIndividual> getOWLIndividuals(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLNamedIndividual> getOWLIndividuals(String str, int limit) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLDatatype> getOWLDatatypes(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLEntity> getOWLProperties(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLEntity> getOWLEntities(String str) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public Set<OWLEntity> getOWLEntities(String str, OWLOntology ont) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

    @Override
    public <T extends OWLEntity> Set<T> getOWLEntities(String str, EntityType<T> type, OWLOntology ont) {
        throw new NotImplemented("DBEntityFinder search not implemented yet");
    }

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

    @Override
    public void dispose() {
        // No-op
    }
}
