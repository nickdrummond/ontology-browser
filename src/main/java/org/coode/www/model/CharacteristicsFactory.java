package org.coode.www.model;

import com.google.common.base.Optional;
import com.google.common.collect.Sets;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.*;

public class CharacteristicsFactory {

    public Optional<Characteristic> getAnnotations(OWLEntity owlEntity, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLAnnotation> annots = new ArrayList<>();
        for (OWLOntology ont : ontologies){
            annots.addAll(EntitySearcher.getAnnotations(owlEntity.getIRI(), ont));
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
        List<OWLClassExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentClasses(owlClass, ontologies));
        equivs.remove(owlClass);
        return asCharacteristic("Equivalents", owlClass, equivs, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentProperties(owlObjectProperty, ontologies));
        equivs.remove(owlObjectProperty);
        return asCharacteristic("Equivalents", owlObjectProperty, equivs, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentProperties(owlDataProperty, ontologies));
        equivs.remove(owlDataProperty);
        return asCharacteristic("Equivalents", owlDataProperty, equivs, comparator);
    }

    public Optional<Characteristic> getSuperclasses(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> clses = new ArrayList<>(EntitySearcher.getSuperClasses(owlClass, ontologies));
        return asCharacteristic("Superclasses", owlClass, clses, comparator);
    }

    public Optional<Characteristic> getSupers(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> supers = new ArrayList<>(EntitySearcher.getSuperProperties(owlObjectProperty, ontologies));
        return asCharacteristic("Superproperties", owlObjectProperty, supers, comparator);
    }

    public Optional<Characteristic> getSupers(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> supers = new ArrayList<>(EntitySearcher.getSuperProperties(owlDataProperty, ontologies));
        return asCharacteristic("Superproperties", owlDataProperty, supers, comparator);
    }

    public Optional<Characteristic> getSupers(OWLAnnotationProperty owlAnnotationProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLAnnotationProperty> supers = new ArrayList<>(EntitySearcher.getSuperProperties(owlAnnotationProperty, ontologies));
        return asCharacteristic("Superproperties", owlAnnotationProperty, supers, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> disjoints = new ArrayList<>(EntitySearcher.getDisjointClasses(owlClass, ontologies));
        disjoints.remove(owlClass);
        return asCharacteristic("Disjoints", owlClass, disjoints, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> disjoints = new ArrayList<>(EntitySearcher.getDisjointProperties(owlObjectProperty, ontologies));
        disjoints.remove(owlObjectProperty);
        return asCharacteristic("Disjoints", owlObjectProperty, disjoints, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> disjoints = new ArrayList<>(EntitySearcher.getDisjointProperties(owlDataProperty, ontologies));
        disjoints.remove(owlDataProperty);
        return asCharacteristic("Disjoints", owlDataProperty, disjoints, comparator);
    }

    public Optional<Characteristic> getMembers(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> members = new ArrayList<>(EntitySearcher.getInstances(owlClass, ontologies));
        return asCharacteristic("Members", owlClass, members, comparator);
    }

    public Optional<Characteristic> getDomains(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> domains = new ArrayList<>(EntitySearcher.getDomains(owlObjectProperty, ontologies));
        return asCharacteristic("Domains", owlObjectProperty, domains, comparator);
    }

    public Optional<Characteristic> getDomains(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> domains = new ArrayList<>(EntitySearcher.getDomains(owlDataProperty, ontologies));
        return asCharacteristic("Domains", owlDataProperty, domains, comparator);
    }

    public Optional<Characteristic> getDomains(OWLAnnotationProperty owlAnnotationProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<IRI> domains = new ArrayList<>(EntitySearcher.getDomains(owlAnnotationProperty, ontologies));
        return asCharacteristic("Domains", owlAnnotationProperty, domains, comparator);
    }

    public Optional<Characteristic> getRanges(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> ranges = new ArrayList<>(EntitySearcher.getRanges(owlObjectProperty, ontologies));
        return asCharacteristic("Ranges", owlObjectProperty, ranges, comparator);
    }

    public Optional<Characteristic> getRanges(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataRange> ranges = new ArrayList<>(EntitySearcher.getRanges(owlDataProperty, ontologies));
        return asCharacteristic("Ranges", owlDataProperty, ranges, comparator);
    }

    public Optional<Characteristic> getRanges(OWLAnnotationProperty owlAnnotationProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<IRI> ranges = new ArrayList<>(EntitySearcher.getRanges(owlAnnotationProperty, ontologies));
        return asCharacteristic("Ranges", owlAnnotationProperty, ranges, comparator);
    }

    public Optional<Characteristic> getInverses(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> inverses = new ArrayList<>(EntitySearcher.getInverses(owlObjectProperty, ontologies));
        return asCharacteristic("Inverses", owlObjectProperty, inverses, comparator);
    }

    public Optional<Characteristic> getUsage(OWLEntity owlEntity, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        UsageVisibilityVisitor usageVisibilityVisitor = new UsageVisibilityVisitor();
        List<OWLObject> usage = new ArrayList<>();
        for (OWLOntology ont : ontologies){
            for (OWLAxiom ax : ont.getReferencingAxioms(owlEntity)){
                if (usageVisibilityVisitor.getShowUsage(ax, owlEntity)){
                    usage.add(ax);
                }
            }
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
        List<OWLClassExpression> types = new ArrayList<>(EntitySearcher.getTypes(owlIndividual, ontologies));
        return asCharacteristic("Types", owlIndividual, types, comparator);
    }

    public Optional<Characteristic> getSameAs(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> sameAs = new ArrayList<>(EntitySearcher.getSameIndividuals(owlIndividual, ontologies));
        sameAs.remove(owlIndividual);
        return asCharacteristic("Same As", owlIndividual, sameAs, comparator);
    }

    public Optional<Characteristic> getDifferentFrom(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> differentFrom = new ArrayList<>(EntitySearcher.getDifferentIndividuals(owlIndividual, ontologies));
        differentFrom.remove(owlIndividual);
        return asCharacteristic("Different From", owlIndividual, differentFrom, comparator);
    }

    public Collection<? extends Characteristic> getObjectPropertyAssertions(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator,
            ShortFormProvider shortFormProvider) {

        final Map<OWLObjectPropertyExpression, Collection<OWLIndividual>> propMap =
                EntitySearcher.getObjectPropertyValues(owlIndividual, ontologies).asMap();

        final List<OWLObjectPropertyExpression> orderedProps = new ArrayList<>(propMap.keySet());

        Collections.sort(orderedProps, comparator);

        List<Characteristic> characteristics = new ArrayList<>();

        for (OWLObjectPropertyExpression p : orderedProps) {
            // TODO improved expression rendering
            String label = p.isAnonymous() ? p.toString() : shortFormProvider.getShortForm(p.asOWLObjectProperty());
            characteristics.add(new Characteristic(
                            owlIndividual,
                            label,
                            new ArrayList<>(propMap.get(p)))
            );
        }

        return characteristics;
    }

    public Collection<? extends Characteristic> getDataPropertyAssertions(
            OWLNamedIndividual owlIndividual,
            Set<OWLOntology> ontologies,
            Comparator<OWLObject> comparator,
            ShortFormProvider shortFormProvider) {

        final Map<OWLDataPropertyExpression, Collection<OWLLiteral>> propMap =
                EntitySearcher.getDataPropertyValues(owlIndividual, ontologies).asMap();

        final List<OWLDataPropertyExpression> orderedProps = new ArrayList<>(propMap.keySet());

        Collections.sort(orderedProps, comparator);

        List<Characteristic> characteristics = new ArrayList<>();

        for (OWLDataPropertyExpression p : orderedProps) {
            // TODO improved expression rendering
            String label = p.isAnonymous() ? p.toString() : shortFormProvider.getShortForm(p.asOWLDataProperty());
            characteristics.add(new Characteristic(
                            owlIndividual,
                            label,
                            new ArrayList<>(propMap.get(p)))
            );
        }

        return characteristics;
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
                Set<OWLAnnotationValue> objects = props.get(p);
                if (objects == null){
                    objects = Sets.newHashSet();
                    props.put(p, objects);
                }
                objects.add(ax.getAnnotation().getValue());
            }
        }
        return props;
    }

    private Optional<Characteristic> asCharacteristic(String name, OWLObject owlObject, List<? extends OWLObject> results, Comparator<OWLObject> comparator) {
        if (!results.isEmpty()) {
            Collections.sort(results, comparator);
            return Optional.of(new Characteristic(owlObject, name, results));
        }
        return Optional.absent();
    }
}
