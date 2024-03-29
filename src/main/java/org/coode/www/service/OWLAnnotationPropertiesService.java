package org.coode.www.service;

import com.google.common.collect.Sets;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.AnnotationPropertyCharacteristicsBuilder;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.coode.www.service.hierarchy.AnnotationPropComparator;
import org.coode.www.service.hierarchy.AnnotationsHierarchyService;
import org.coode.www.service.hierarchy.OWLAnnotationPropertyHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

@Service
public class OWLAnnotationPropertiesService implements PropertiesService<OWLAnnotationProperty> {

    public OWLAnnotationProperty getPropertyFor(final String propertyId, final OWLOntology ont) throws NotFoundException {
        for (OWLOntology o : ont.getImportsClosure()){
            for (OWLAnnotationProperty owlAnnotationProperty: o.getAnnotationPropertiesInSignature()) {
                if (getIdFor(owlAnnotationProperty).equals(propertyId)){
                    return owlAnnotationProperty;
                }
            }
        }
        throw new NotFoundException("OWLAnnotationProperty", propertyId);
    }

    public String getIdFor(final OWLAnnotationProperty owlAnnotationProperty) {
        return String.valueOf(owlAnnotationProperty.getIRI().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLAnnotationProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new AnnotationPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize).getCharacteristics();
    }

    @Override
    public Comparator<Tree<OWLNamedIndividual>> getComparator(OWLAnnotationProperty orderByProperty, OWLOntology ont) {
        if (orderByProperty != null) {
            return new AnnotationPropComparator(orderByProperty, ont);
        }
        return Comparator.comparing(o -> o.value.iterator().next());
    }

    @Override
    public Tree<? extends OWLObject> getPropTree(OWLAnnotationProperty property, OWLOntology ont) {
        OWLAnnotationPropertyHierarchyService hierarchyService =
                new OWLAnnotationPropertyHierarchyService(ont, Comparator.comparing(o -> o.value.iterator().next()));
        return hierarchyService.getPrunedTree(property);
    }

    @Override
    public AbstractRelationsHierarchyService<OWLAnnotationProperty> getRelationsHierarchy(Comparator<Tree<OWLNamedIndividual>> comparator) {
        return new AnnotationsHierarchyService(comparator);
    }

    public List<OWLAnnotationProperty> getAnnotationProperties(final OWLOntology activeOntology,
                                                               final Comparator<OWLObject> comparator) {
        Set<OWLAnnotationProperty> props = Sets.newHashSet();
        for (OWLOntology ont : activeOntology.getImportsClosure()) {
            props.addAll(ont.getAnnotationPropertiesInSignature());
        }
        return props.stream().sorted(comparator).collect(Collectors.toList());
    }
}
