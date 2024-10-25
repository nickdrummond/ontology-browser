package org.ontbrowser.www.feature.entities;

import org.apache.commons.lang3.NotImplementedException;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.DataPropertyCharacteristicsBuilder;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.service.hierarchy.*;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLDataPropertiesService implements PropertiesService<OWLDataProperty> {

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @Override
    public OWLDataProperty getPropertyFor(String propertyId, OWLOntology ont) throws NotFoundException {
        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();
        if (getIdFor(owlTopDataProperty).equals(propertyId)) {
            return owlTopDataProperty;
        }

        OWLDataProperty owlBottomDataProperty = df.getOWLBottomDataProperty();
        if (getIdFor(owlBottomDataProperty).equals(propertyId)) {
            return owlBottomDataProperty;
        }

        for (OWLOntology o : ont.getImportsClosure()){
            for (OWLDataProperty owlDataProperty: o.getDataPropertiesInSignature()) {
                if (getIdFor(owlDataProperty).equals(propertyId)){
                    return owlDataProperty;
                }
            }
        }
        throw new NotFoundException("OWLDataProperty", propertyId);
    }

    public String getIdFor(final OWLDataProperty owlDataProperty) {
        return String.valueOf(owlDataProperty.getIRI().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLDataProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new DataPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize).getCharacteristics();
    }

    @Override
    public OWLHierarchyService<OWLDataProperty> getHierarchyService(OWLOntology ont) {
        return new OWLDataPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                Comparator.comparing(o -> o.value.iterator().next()));
    }

    @Override
    public Comparator<Tree<OWLNamedIndividual>> getComparator(OWLDataProperty orderByProperty, OWLOntology ont) {
        throw new NotImplementedException("Not implemented");
    }

    @Override
    public AbstractRelationsHierarchyService<OWLDataProperty> getRelationsHierarchy(Comparator<Tree<OWLNamedIndividual>> comparator) {
        throw new NotImplementedException("Not implemented");
    }
}
