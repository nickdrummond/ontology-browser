package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.ObjectPropertyCharacteristicsBuilder;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.service.hierarchy.*;
import org.semanticweb.owlapi.model.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.util.Comparator;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;
import static org.springframework.http.HttpStatus.NOT_FOUND;

@Service
public class OWLObjectPropertiesService implements PropertiesService<OWLObjectProperty> {

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @Override
    public OWLObjectProperty getPropertyFor(String propertyId, OWLOntology ont) {
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
        throw new ResponseStatusException(NOT_FOUND, "OWLObjectProperty not found: " + propertyId);
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
        return treeComparator();
    }

    @Override
    public OWLHierarchyService<OWLObjectPropertyExpression> getHierarchyService(OWLOntology ont) {
        return new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                treeComparator());
    }

    public AbstractRelationsHierarchyService<OWLObjectProperty> getRelationsHierarchy (
            Comparator<Tree<OWLNamedIndividual>> comparator) {
        return new RelationsHierarchyService(comparator);
    }
}
