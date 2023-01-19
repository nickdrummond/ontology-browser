package org.coode.www.model;

import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class CharacteristicsFactory {

    public Optional<Characteristic> getAnnotations(OWLOntology ont, Comparator<OWLObject> c) {
        return asCharacteristicNew("Annotations", ont,
                wrapWithOntology(ont.getAnnotations(), ont, c));
    }

    public Optional<Characteristic> getImports(OWLOntology ont, Comparator<OWLObject> c) {
        return asCharacteristicNew("Imports", ont,
                wrapWithOntology(ont.getDirectImportsDocuments(), ont, c));
    }

    public Optional<Characteristic> getGeneralClassAxioms(OWLOntology ont, Comparator<OWLObject> c) {
        return asCharacteristicNew("General Class Axioms", ont,
                wrapWithOntology(ont.getGeneralClassAxioms(), ont, c));
    }

    public Optional<Characteristic> getAnnotations(OWLEntity ent, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Annotations", ent, wrap(onts, c,
                ont -> EntitySearcher.getAnnotations(ent, ont)));
    }

    public Optional<Characteristic> getEquivalents(OWLClass cls, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Equivalents", cls, wrap(onts, c,
                ont -> EntitySearcher.getEquivalentClasses(cls, ont).filter(e -> !e.equals(cls))));
    }

    public Optional<Characteristic> getEquivalents(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Equivalents", prop, wrap(onts, c,
                ont -> EntitySearcher.getEquivalentProperties(prop, ont).filter(e -> !e.equals(prop))));
    }

    public Optional<Characteristic> getEquivalents(OWLDataProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Equivalents", prop, wrap(onts, c,
                ont -> EntitySearcher.getEquivalentProperties(prop, ont).filter(e -> !e.equals(prop))));
    }

    public Optional<Characteristic> getSuperclasses(OWLClass cls, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Superclasses", cls, wrap(onts, c,
                ont -> EntitySearcher.getSuperClasses(cls, ont)));
    }

    public Optional<Characteristic> getSupers(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Superproperties", prop, wrap(onts, c,
                ont -> EntitySearcher.getSuperProperties(prop, ont)));
    }

    public Optional<Characteristic> getSupers(OWLDataProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Superproperties", prop, wrap(onts, c,
                ont -> EntitySearcher.getSuperProperties(prop, ont)));
    }

    public Optional<Characteristic> getSupers(OWLAnnotationProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Superproperties", prop, wrap(onts, c,
                ont -> EntitySearcher.getSuperProperties(prop, ont)));
    }

    public Optional<Characteristic> getDisjoints(OWLClass cls, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Disjoints", cls, wrap(onts, c,
                ont -> EntitySearcher.getDisjointClasses(cls, ont).filter(e -> !e.equals(cls))));
    }

    public Optional<Characteristic> getDisjoints(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Disjoints", prop, wrap(onts, c,
                ont -> EntitySearcher.getDisjointProperties(prop, ont).filter(e -> !e.equals(prop))));
    }

    public Optional<Characteristic> getDisjoints(OWLDataProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Disjoints", prop, wrap(onts, c,
                ont -> EntitySearcher.getDisjointProperties(prop, ont).filter(e -> !e.equals(prop))));
    }

    public Optional<Characteristic> getMembers(OWLClass cls, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Members", cls, wrap(onts, c,
                ont -> EntitySearcher.getInstances(cls, ont)));
    }

    public Optional<Characteristic> getDomains(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Domains", prop, wrap(onts, c,
                ont -> EntitySearcher.getDomains(prop, ont)));        
    }

    public Optional<Characteristic> getDomains(OWLDataProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Domains", prop, wrap(onts, c,
                ont -> EntitySearcher.getDomains(prop, ont)));
    }

    public Optional<Characteristic> getDomains(OWLAnnotationProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Domains", prop, wrap(onts, c,
                ont -> EntitySearcher.getDomains(prop, ont)));
    }

    public Optional<Characteristic> getRanges(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Ranges", prop, wrap(onts, c,
                ont -> EntitySearcher.getRanges(prop, ont)));
    }

    public Optional<Characteristic> getRanges(OWLDataProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Ranges", prop, wrap(onts, c,
                ont -> EntitySearcher.getRanges(prop, ont)));
    }

    public Optional<Characteristic> getRanges(OWLAnnotationProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Ranges", prop, wrap(onts, c,
                ont -> EntitySearcher.getRanges(prop, ont)));
    }

    public Optional<Characteristic> getInverses(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Inverses", prop, wrap(onts, c,
                ont -> EntitySearcher.getInverses(prop, ont)));
    }

    public Optional<Characteristic> getUsage(OWLEntity ent, Set<OWLOntology> onts, Comparator<OWLObject> c, UsageVisibilityVisitor vis) {
        // Using the IRI as ont.referencingAxioms() doesn't pick up annotations that have this entity IRI as a value.
        // This means puns will also show up in the usage which is probably useful for the user.
        return asCharacteristicNew("Usage", ent, wrap(onts, c,
                ont -> ont.referencingAxioms(ent.getIRI(), Imports.EXCLUDED).filter(ax -> vis.getShowUsage(ax, ent))));
    }

    public Optional<Characteristic> getPropertyCharacteristics(OWLObjectProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Characteristics", prop, wrap(onts, c,
                ont -> ont.referencingAxioms(prop, Imports.EXCLUDED).filter(ax -> ax instanceof OWLObjectPropertyCharacteristicAxiom)));
    }

    public Optional<Characteristic> getPropertyCharacteristics(OWLDataProperty prop, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Characteristics", prop, wrap(onts, c,
                ont -> ont.referencingAxioms(prop, Imports.EXCLUDED).filter(ax -> ax instanceof OWLDataPropertyCharacteristicAxiom)));
    }

    public Optional<Characteristic> getDatatypeDefinitions(OWLDatatype dt, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Datatype Definitions", dt, wrap(onts, c,
                ont -> ont.datatypeDefinitions(dt).map(OWLDatatypeDefinitionAxiom::getDatatype)));
    }

    public Optional<Characteristic> getSameAs(OWLIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Same As", ind, wrap(onts, c,
                ont -> EntitySearcher.getSameIndividuals(ind, ont).filter(e -> !e.equals(ind))));
    }

    public Optional<Characteristic> getDifferentFrom(OWLIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Different From", ind, wrap(onts, c,
                ont -> EntitySearcher.getDifferentIndividuals(ind, ont).filter(e -> !e.equals(ind))));
    }

    public Optional<Characteristic> getTypes(OWLIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Types", ind, wrap(onts, c,
                ont -> EntitySearcher.getTypes(ind, ont)));
    }

    public Optional<Characteristic> getObjectPropertyAssertions(OWLNamedIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Object Property Assertions", ind, wrap(onts, c,
                ont -> ont.objectPropertyAssertionAxioms(ind)));
    }

    public Optional<Characteristic> getDataPropertyAssertions(OWLNamedIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Data Property Assertions", ind, wrap(onts, c,
                ont -> ont.dataPropertyAssertionAxioms(ind)));
    }

    public Optional<Characteristic> getNegativeObjectPropertyAssertions(OWLNamedIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Negative Object Property Assertions", ind, wrap(onts, c,
                ont -> ont.negativeObjectPropertyAssertionAxioms(ind)));
    }

    public Optional<Characteristic> getNegativeDataPropertyAssertions(OWLNamedIndividual ind, Set<OWLOntology> onts, Comparator<OWLObject> c) {
        return asCharacteristicNew("Negative Data Property Assertions", ind, wrap(onts, c,
                ont -> ont.negativeObjectPropertyAssertionAxioms(ind)));
    }

    public List<Characteristic> getAnnotationCharacteristics(final OWLNamedIndividual ind,
                                                             final Set<OWLOntology> onts,
                                                             final Comparator<OWLObject> comp,
                                                             final ShortFormProvider sfp) {

        final Map<OWLAnnotationProperty, List<OWLObjectWithOntology>> assertedProps = new HashMap<>();

        for (OWLOntology ont : onts){
            for (OWLAnnotationAssertionAxiom ax : ont.getAnnotationAssertionAxioms(ind.getIRI())) {
                OWLAnnotationProperty p = ax.getProperty();
                List<OWLObjectWithOntology> objects = assertedProps.computeIfAbsent(p, k -> new ArrayList<>());
                objects.add(new OWLObjectWithOntology(ax.getAnnotation().getValue(), ont));
            }
        }

        return assertedProps.keySet().stream()
                .sorted(comp)
                .map(p -> asCharacteristicNew(sfp.getShortForm(p), ind, assertedProps.get(p)).get())
                .collect(Collectors.toList());
    }

    /* All ontology queries return collections of OWLObjects - we want to wrap these with the ontology the assertions
     * are in
     */
    private List<OWLObjectWithOntology> wrap(final Set<OWLOntology> onts,
                                             final Comparator<OWLObject> c,
                                             final Function<OWLOntology, Stream<? extends OWLObject>> f) {
        return onts.stream()
                .flatMap(o -> f.apply(o).map(ax -> new OWLObjectWithOntology(ax, o)))
                .sorted((o1, o2) -> c.compare(o1.getOWLObject(), o2.getOWLObject()))
                .collect(Collectors.toList());
    }

    private List<OWLObjectWithOntology> wrapWithOntology(Set<? extends OWLObject> objs, OWLOntology ont, Comparator<OWLObject> c) {
        return objs.stream()
                .sorted(c)
                .map(o -> new OWLObjectWithOntology(o, ont))
                .collect(Collectors.toList());
    }

    private Optional<Characteristic> asCharacteristicNew(String name, OWLObject owlObject, List<OWLObjectWithOntology> results) {
        return results.isEmpty() ? Optional.empty() : Optional.of(new Characteristic(owlObject, name, results));
    }
}
