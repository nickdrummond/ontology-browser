package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.characteristics.AnnotationPropertyCharacteristicsBuilder;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.service.hierarchy.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.stereotype.Service;

import java.util.*;

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
    public OWLHierarchyService<OWLAnnotationProperty> getHierarchyService(OWLOntology ont) {
        return new OWLAnnotationPropertyHierarchyService(ont, Comparator.comparing(o -> o.value.iterator().next()));
    }

    @Override
    public AbstractRelationsHierarchyService<OWLAnnotationProperty> getRelationsHierarchy(Comparator<Tree<OWLNamedIndividual>> comparator) {
        return new AnnotationsHierarchyService(comparator);
    }

    public List<OWLAnnotationProperty> getAnnotationProperties(final OWLOntology ont,
                                                               final Comparator<OWLObject> comparator) {
        return ont.getAnnotationPropertiesInSignature(Imports.INCLUDED).stream().sorted(comparator).toList();
    }
}
