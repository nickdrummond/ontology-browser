package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.ObjectPropertyCharacteristicsBuilder;
import org.coode.www.service.hierarchy.*;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLObjectPropertiesService implements PropertiesService<OWLObjectProperty>{

    @Autowired
    private ReasonerFactoryService reasonerFactoryService;

    @Override
    public OWLObjectProperty getPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        //TODO this should be cached
        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        if (getIdFor(owlTopObjectProperty).equals(propertyId)) {
            return owlTopObjectProperty;
        }

        OWLObjectProperty owlBottomObjectProperty = df.getOWLBottomObjectProperty();
        if (getIdFor(owlBottomObjectProperty).equals(propertyId)) {
            return owlBottomObjectProperty;
        }
        for (OWLOntology ont : kit.getActiveOntologies()){
            for (OWLObjectProperty owlObjectProperty: ont.getObjectPropertiesInSignature()) {
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
    public List<Characteristic> getCharacteristics(final OWLObjectProperty property, final OWLHTMLKit kit) {
        return new ObjectPropertyCharacteristicsBuilder(property, kit.getActiveOntologies(), kit.getComparator()).getCharacteristics();
    }

    @Override
    public Comparator<Tree<Relation<OWLObjectProperty>>> getComparator(OWLObjectProperty orderByProperty, OWLOntology ont) {
        if (orderByProperty != null) {
            return new PropComparator(orderByProperty, ont);
        }
        return Comparator.comparing(o -> o.value.iterator().next().individual());
    }

    @Override
    public Tree<? extends OWLObject> getPropTree(OWLObjectProperty property, OWLOntology ont) {
        OWLObjectPropertyHierarchyService hierarchyService = new OWLObjectPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                Comparator.comparing(o -> o.value.iterator().next()));
        return hierarchyService.getPrunedTree(property);
    }

    public AbstractRelationsHierarchyService<OWLObjectProperty> getRelationsHierarchy (Comparator<Tree<Relation<OWLObjectProperty>>> comparator) {
        return new RelationsHierarchyService(comparator);
    }

    @Override
    public boolean isEquivalentOrSubproperty(OWLObjectPropertyExpression property, OWLObjectProperty superProperty, OWLOntology ont) {
        OWLReasoner toldReasoner = reasonerFactoryService.getToldReasoner(ont);
        return property.equals(superProperty) ||
                toldReasoner.getEquivalentObjectProperties(superProperty).contains(property) ||
                toldReasoner.getSubObjectProperties(superProperty).containsEntity(property);
    }
}
