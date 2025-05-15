package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.AnnotationPropertyCharacteristicsBuilder;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.hierarchy.*;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;

@Service
public class OWLAnnotationPropertiesService implements PropertiesService<OWLAnnotationProperty> {

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
        return treeComparator();
    }

    @Override
    public OWLHierarchyService<OWLAnnotationProperty> getHierarchyService(OWLOntology ont) {
        return new OWLAnnotationPropertyHierarchyService(ont, treeComparator());
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
