package org.coode.www.model;

import com.google.common.base.Optional;
import com.google.common.collect.Multimap;
import org.coode.owl.util.OWLUtils;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
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

    public Optional<Characteristic> getEquivalents(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentClasses(owlClass, ontologies));
        return asCharacteristic("Equivalents", owlClass, equivs, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentProperties(owlObjectProperty, ontologies));
        return asCharacteristic("Equivalents", owlObjectProperty, equivs, comparator);
    }

    public Optional<Characteristic> getEquivalents(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> equivs = new ArrayList<>(EntitySearcher.getEquivalentProperties(owlDataProperty, ontologies));
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

    public Optional<Characteristic> getDisjoints(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> disjoints = new ArrayList<>(EntitySearcher.getDisjointClasses(owlClass, ontologies));
        return asCharacteristic("Disjoints", owlClass, disjoints, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLObjectPropertyExpression> disjoints = new ArrayList<>(EntitySearcher.getDisjointProperties(owlObjectProperty, ontologies));
        return asCharacteristic("Disjoints", owlObjectProperty, disjoints, comparator);
    }

    public Optional<Characteristic> getDisjoints(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataPropertyExpression> disjoints = new ArrayList<>(EntitySearcher.getDisjointProperties(owlDataProperty, ontologies));
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
    
    public Optional<Characteristic> getRanges(OWLObjectProperty owlObjectProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> ranges = new ArrayList<>(EntitySearcher.getRanges(owlObjectProperty, ontologies));
        return asCharacteristic("Ranges", owlObjectProperty, ranges, comparator);
    }

    public Optional<Characteristic> getRanges(OWLDataProperty owlDataProperty, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLDataRange> ranges = new ArrayList<>(EntitySearcher.getRanges(owlDataProperty, ontologies));
        return asCharacteristic("Ranges", owlDataProperty, ranges, comparator);
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

    public Optional<Characteristic> getTypes(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> types = new ArrayList<>(EntitySearcher.getTypes(owlIndividual, ontologies));
        return asCharacteristic("Types", owlIndividual, types, comparator);
    }

    public Optional<Characteristic> getSameAs(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> sameAs = new ArrayList<>(EntitySearcher.getSameIndividuals(owlIndividual, ontologies));
        return asCharacteristic("Same As", owlIndividual, sameAs, comparator);
    }

    public Optional<Characteristic> getDifferentFrom(OWLIndividual owlIndividual, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> differentFrom = new ArrayList<>(EntitySearcher.getDifferentIndividuals(owlIndividual, ontologies));
        return asCharacteristic("Different From", owlIndividual, differentFrom, comparator);
    }

    private Optional<Characteristic> asCharacteristic(String name, OWLObject owlObject, List<? extends OWLObject> results, Comparator<OWLObject> comparator) {
        if (!results.isEmpty()) {
            Collections.sort(results, comparator);
            return Optional.of(new Characteristic(owlObject, name, results));
        }
        return Optional.absent();
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
        Map<OWLAnnotationProperty, Set<OWLAnnotationValue>> props = new HashMap<OWLAnnotationProperty, Set<OWLAnnotationValue>>();
        for (OWLOntology ont : onts){
            for (OWLAnnotationAssertionAxiom ax : ont.getAnnotationAssertionAxioms(individual.getIRI())){
                OWLAnnotationProperty p = ax.getProperty();
                Set<OWLAnnotationValue> objects = props.get(p);
                if (objects == null){
                    objects = new HashSet<>();
                    props.put(p, objects);
                }
                objects.add(ax.getAnnotation().getValue());
            }
        }
        return props;
    }
}
