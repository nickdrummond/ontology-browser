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
        List<OWLClassExpression> clses = new ArrayList<>(EntitySearcher.getEquivalentClasses(owlClass, ontologies));
        return asCharacteristic("Equivalents", owlClass, clses, comparator);
    }

    public Optional<Characteristic> getSuperclasses(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> clses = new ArrayList<>(EntitySearcher.getSuperClasses(owlClass, ontologies));
        return asCharacteristic("Superclasses", owlClass, clses, comparator);
    }

    public Optional<Characteristic> getDisjointClasses(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLClassExpression> clses = new ArrayList<>(EntitySearcher.getDisjointClasses(owlClass, ontologies));
        return asCharacteristic("Disjoints", owlClass, clses, comparator);
    }

    public Optional<Characteristic> getMembers(OWLClass owlClass, Set<OWLOntology> ontologies, Comparator<OWLObject> comparator) {
        List<OWLIndividual> members = new ArrayList<>(EntitySearcher.getInstances(owlClass, ontologies));
        return asCharacteristic("Members", owlClass, members, comparator);
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
        return asCharacteristic("Annotations", owlEntity, usage, comparator);
    }

    private Optional<Characteristic> asCharacteristic(String name, OWLObject owlObject, List<? extends OWLObject> results, Comparator<OWLObject> comparator) {
        if (!results.isEmpty()) {
            Collections.sort(results, comparator);
            return Optional.of(new Characteristic(owlObject, name, results));
        }
        return Optional.absent();
    }
}
