package org.coode.www.model;

import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.*;
import java.util.stream.Collectors;

public class CharacteristicsFactory {

    public Optional<Characteristic> getAnnotations(OWLEntity owlEntity, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLAnnotation> annots = new ArrayList<>();
        for (OWLOntology ont : ontologies){
            annots.addAll(EntitySearcher.getAnnotations(owlEntity.getIRI(), ont).collect(Collectors.toList()));
        }
        return asCharacteristic("Annotations", owlEntity, annots, comparator);
    }

    public Optional<Characteristic> getAnnotations(OWLOntology owlOntology, Comparator<OWLObject> comparator) {
        List<OWLAnnotation> annotations = new ArrayList<>(owlOntology.getAnnotations());
        return asCharacteristic("Annotations", owlOntology, annotations, comparator);
    }

    public Optional<Characteristic> getImports(OWLOntology owlOntology, Comparator<OWLObject> comparator) {
        List<IRI> imports = new ArrayList<>(owlOntology.getDirectImportsDocuments());
        return asCharacteristic("Imports", owlOntology, imports, comparator);
    }

    public Optional<Characteristic> getGeneralClassAxioms(OWLOntology owlOntology, Comparator<OWLObject> comparator) {
        List<OWLClassAxiom> axioms = new ArrayList<>(owlOntology.getGeneralClassAxioms());
        return asCharacteristic("General Class Axioms", owlOntology, axioms, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> equivs = EntitySearcher.getEquivalentClasses(owlClass, ontologies.stream()).collect(Collectors.toList());
        equivs.remove(owlClass);
        return asCharacteristic("Equivalents", owlClass, equivs, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> equivs = EntitySearcher.getEquivalentProperties(owlObjectProperty, ontologies.stream()).collect(Collectors.toList());
        equivs.remove(owlObjectProperty);
        return asCharacteristic("Equivalents", owlObjectProperty, equivs, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> equivs = EntitySearcher.getEquivalentProperties(owlDataProperty, ontologies.stream()).collect(Collectors.toList());
        equivs.remove(owlDataProperty);
        return asCharacteristic("Equivalents", owlDataProperty, equivs, comparator);
    }

    public Optional<Characteristic> getSuperclasses(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> clses = EntitySearcher.getSuperClasses(owlClass, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Superclasses", owlClass, clses, comparator);
    }

    public Optional<Characteristic> getSupers(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> supers = EntitySearcher.getSuperProperties(owlObjectProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Superproperties", owlObjectProperty, supers, comparator);
    }

    public Optional<Characteristic> getSupers(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> supers = EntitySearcher.getSuperProperties(owlDataProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Superproperties", owlDataProperty, supers, comparator);
    }

    public Optional<Characteristic> getSupers(OWLAnnotationProperty owlAnnotationProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLAnnotationProperty> supers = EntitySearcher.getSuperProperties(owlAnnotationProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Superproperties", owlAnnotationProperty, supers, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> disjoints = EntitySearcher.getDisjointClasses(owlClass, ontologies.stream()).collect(Collectors.toList());
        disjoints.remove(owlClass);
        return asCharacteristic("Disjoints", owlClass, disjoints, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> disjoints = EntitySearcher.getDisjointProperties(owlObjectProperty, ontologies.stream()).collect(Collectors.toList());
        disjoints.remove(owlObjectProperty);
        return asCharacteristic("Disjoints", owlObjectProperty, disjoints, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> disjoints = EntitySearcher.getDisjointProperties(owlDataProperty, ontologies.stream()).collect(Collectors.toList());
        disjoints.remove(owlDataProperty);
        return asCharacteristic("Disjoints", owlDataProperty, disjoints, comparator);
    }

    public Optional<Characteristic> getMembers(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> members = EntitySearcher.getInstances(owlClass, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Members", owlClass, members, comparator);
    }

    public Optional<Characteristic> getDomains(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> domains = EntitySearcher.getDomains(owlObjectProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Domains", owlObjectProperty, domains, comparator);
    }

    public Optional<Characteristic> getDomains(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> domains = EntitySearcher.getDomains(owlDataProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Domains", owlDataProperty, domains, comparator);
    }

    public Optional<Characteristic> getDomains(OWLAnnotationProperty owlAnnotationProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<IRI> domains = EntitySearcher.getDomains(owlAnnotationProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Domains", owlAnnotationProperty, domains, comparator);
    }

    public Optional<Characteristic> getRanges(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> ranges = EntitySearcher.getRanges(owlObjectProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Ranges", owlObjectProperty, ranges, comparator);
    }

    public Optional<Characteristic> getRanges(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataRange> ranges = EntitySearcher.getRanges(owlDataProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Ranges", owlDataProperty, ranges, comparator);
    }

    public Optional<Characteristic> getRanges(OWLAnnotationProperty owlAnnotationProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<IRI> ranges = EntitySearcher.getRanges(owlAnnotationProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Ranges", owlAnnotationProperty, ranges, comparator);
    }

    public Optional<Characteristic> getInverses(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> inverses = EntitySearcher.getInverses(owlObjectProperty, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Inverses", owlObjectProperty, inverses, comparator);
    }

    public Optional<Characteristic> getUsage(OWLEntity owlEntity, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        UsageVisibilityVisitor usageVisibilityVisitor = new UsageVisibilityVisitor();
        List<OWLObject> usage = new ArrayList<>();
        for (OWLOntology ont : ontologies){
            for (OWLAxiom ax : ont.getReferencingAxioms(owlEntity, Imports.EXCLUDED)){
                if (usageVisibilityVisitor.getShowUsage(ax, owlEntity)){
                    usage.add(ax);
                }
            }
            // TODO get annotations that have this entity IRI as a value (getRefAxioms doesn't pick these up)
        }
        return asCharacteristic("Usage", owlEntity, usage, comparator);
    }

    public Optional<Characteristic> getPropertyCharacteristics(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyCharacteristicAxiom> axioms = new ArrayList<>();
        for (OWLOntology ont: ontologies) {
            for (OWLAxiom ax : ont.getReferencingAxioms(owlObjectProperty)) {
                if (ax instanceof OWLObjectPropertyCharacteristicAxiom) {
                    axioms.add((OWLObjectPropertyCharacteristicAxiom)ax);
                }
            }
        }
        return asCharacteristic("Characteristics", owlObjectProperty, axioms, comparator);
    }

    public Optional<Characteristic> getPropertyCharacteristics(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyCharacteristicAxiom> axioms = new ArrayList<>();
        for (OWLOntology ont: ontologies) {
            for (OWLAxiom ax : ont.getReferencingAxioms(owlDataProperty)) {
                if (ax instanceof OWLDataPropertyCharacteristicAxiom) {
                    axioms.add((OWLDataPropertyCharacteristicAxiom)ax);
                }
            }
        }
        return asCharacteristic("Characteristics", owlDataProperty, axioms, comparator);
    }

    public Optional<Characteristic> getDatatypeDefinitions(OWLDatatype owlDatatype, Set<OWLOntology> activeOntologies, Comparator<OWLObject> comparator) {
        List<OWLDataRange> ranges = new ArrayList<>();
        for (OWLOntology ont : activeOntologies){
            for (OWLDatatypeDefinitionAxiom ax : ont.getDatatypeDefinitions(owlDatatype)){
                ranges.add(ax.getDataRange());
            }
        }
        return asCharacteristic("Datatype Definitions", owlDatatype, ranges, comparator);
    }

    public Optional<Characteristic> getTypes(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> types = EntitySearcher.getTypes(owlIndividual, ontologies.stream()).collect(Collectors.toList());
        return asCharacteristic("Types", owlIndividual, types, comparator);
    }

    public Optional<Characteristic> getSameAs(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> sameAs = EntitySearcher.getSameIndividuals(owlIndividual, ontologies.stream()).collect(Collectors.toList());
        sameAs.remove(owlIndividual);
        return asCharacteristic("Same As", owlIndividual, sameAs, comparator);
    }

    public Optional<Characteristic> getDifferentFrom(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> differentFrom = EntitySearcher.getDifferentIndividuals(owlIndividual, ontologies.stream()).collect(Collectors.toList());
        differentFrom.remove(owlIndividual);
        return asCharacteristic("Different From", owlIndividual, differentFrom, comparator);
    }

    public Optional<Characteristic> getObjectPropertyAssertions(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator) {

        List<OWLObjectPropertyAssertionAxiom> propAssertions = ontologies.stream().flatMap(
                o -> o.objectPropertyAssertionAxioms(owlIndividual)).sorted(comparator).collect(Collectors.toList());

        return asCharacteristic("Object Property Assertions", owlIndividual, propAssertions, comparator);
    }

    public Optional<Characteristic> getNegativeObjectPropertyAssertions(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator) {

        List<OWLNegativeObjectPropertyAssertionAxiom> propAssertions = ontologies.stream().flatMap(
                o -> o.negativeObjectPropertyAssertionAxioms(owlIndividual)).sorted(comparator).collect(Collectors.toList());

        return asCharacteristic("Negative Object Property Assertions", owlIndividual, propAssertions, comparator);
    }

    public Optional<Characteristic> getDataPropertyAssertions(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator) {

        List<OWLDataPropertyAssertionAxiom> propAssertions = ontologies.stream().flatMap(
                o -> o.dataPropertyAssertionAxioms(owlIndividual)).sorted(comparator).collect(Collectors.toList());

        return asCharacteristic("Data Property Assertions", owlIndividual, propAssertions, comparator);
    }

    public Optional<Characteristic> getNegativeDataPropertyAssertions(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator) {

        List<OWLNegativeDataPropertyAssertionAxiom> propAssertions = ontologies.stream().flatMap(
                o -> o.negativeDataPropertyAssertionAxioms(owlIndividual)).sorted(comparator).collect(Collectors.toList());

        return asCharacteristic("Negative Data Property Assertions", owlIndividual, propAssertions, comparator);
    }

    public List<Characteristic> getAnnotationCharacterists(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator,
            ShortFormProvider shortFormProvider) {

        final Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> assertedProps =
                getAnnotationPropertyMap(owlIndividual, ontologies);

        final List<OWLAnnotationProperty> orderedProps = new ArrayList<>(assertedProps.keySet());

        Collections.sort(orderedProps, comparator);

        List<Characteristic> characteristics = new ArrayList<>();

        for (OWLAnnotationProperty p : orderedProps) {
            characteristics.add(new Characteristic(
                    owlIndividual,
                    shortFormProvider.getShortForm(p),
                    new ArrayList<>(assertedProps.get(p)))
            );
        }

        return characteristics;
    }

    private Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> getAnnotationPropertyMap(
            OWLNamedIndividual individual,
            Set<OWLOntology> onts) {

        Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> props = new HashMap<>();

        for (OWLOntology ont : onts){
            for (OWLAnnotationAssertionAxiom ax : ont.getAnnotationAssertionAxioms(individual.getIRI())) {
                OWLAnnotationProperty p = ax.getProperty();
                Set<OWLAnnotationValue> objects = props.computeIfAbsent(p, k -> new HashSet<>());
                objects.add(ax.getAnnotation().getValue());
            }
        }
        return props;
    }

    private Optional<Characteristic> asCharacteristic(String name, OWLObject owlObject, List<? extends OWLObject> results, Comparator<OWLObject> comparator) {
        if (!results.isEmpty()) {
            results.sort(comparator);
            return Optional.of(new Characteristic(owlObject, name, results));
        }
        return Optional.empty();
    }
}
