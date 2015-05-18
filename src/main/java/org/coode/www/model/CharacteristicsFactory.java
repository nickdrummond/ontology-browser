package org.coode.www.model;

import com.google.common.base.Optional;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

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

    private Optional<Characteristic> asCharacteristic(String name, OWLObject owlObject, List<? extends OWLObject> results, Comparator<OWLObject> comparator) {
        if (!results.isEmpty()) {
            Collections.sort(results, comparator);
            return Optional.of(new Characteristic(owlObject, name, results));
        }
        return Optional.absent();
    }
}
