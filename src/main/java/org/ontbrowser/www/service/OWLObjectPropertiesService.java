package org.ontbrowser.www.service;

import org.apache.commons.lang3.NotImplementedException;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.characteristics.ObjectPropertyCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.service.hierarchy.*;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLObjectPropertyHierarchyService;
import org.ontbrowser.www.service.hierarchy.PropComparator;
import org.ontbrowser.www.service.hierarchy.RelationsHierarchyService;
import org.ontbrowser.www.service.stats.Stats;
import org.ontbrowser.www.service.stats.StatsService;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLObjectPropertiesService implements PropertiesService<OWLObjectProperty>{

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @Override
    public OWLObjectProperty getPropertyFor(String propertyId, OWLOntology ont) throws NotFoundException {
        //TODO this should be cached
        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        if (getIdFor(owlTopObjectProperty).equals(propertyId)) {
            return owlTopObjectProperty;
        }

        OWLObjectProperty owlBottomObjectProperty = df.getOWLBottomObjectProperty();
        if (getIdFor(owlBottomObjectProperty).equals(propertyId)) {
            return owlBottomObjectProperty;
        }
        for (OWLOntology o : ont.getImportsClosure()){
            for (OWLObjectProperty owlObjectProperty: o.getObjectPropertiesInSignature()) {
                if (getIdFor(owlObjectProperty).equals(propertyId)){
                    return owlObjectProperty;
                }
            }
        }
        throw new NotFoundException("OWLObjectProperty", propertyId);
    }

    @Override
    public String getIdFor(final OWLObjectProperty owlObjectProperty) {
        return String.valueOf(owlObjectProperty.getIRI().hashCode());
    }

    @Override
    public List<Characteristic> getCharacteristics(
            final OWLObjectProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new ObjectPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize).getCharacteristics();
    }

    @Override
    public Comparator<Tree<OWLNamedIndividual>> getComparator(
            OWLObjectProperty orderByProperty, OWLOntology ont) {
        if (orderByProperty != null) {
            return new PropComparator(orderByProperty, ont);
        }
        return Comparator.comparing(o -> o.value.iterator().next());
    }

    @Override
    public OWLHierarchyService<OWLObjectPropertyExpression> getHierarchyService(OWLOntology ont) {
        return new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                Comparator.comparing(o -> o.value.iterator().next()));
    }

    public AbstractRelationsHierarchyService<OWLObjectProperty> getRelationsHierarchy (
            Comparator<Tree<OWLNamedIndividual>> comparator) {
        return new RelationsHierarchyService(comparator);
    }
}
