package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLHierarchyService;
import org.ontbrowser.www.model.Tree;
import org.semanticweb.owlapi.model.EntityType;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.util.Comparator;
import java.util.List;

public interface PropertiesService<T extends OWLProperty> extends CharacteristicsProvider<T> {

     Comparator<Tree<OWLNamedIndividual>> getComparator(T orderByProperty, OWLOntology ont);

     OWLHierarchyService<? super T> getHierarchyService(OWLOntology ont);

     AbstractRelationsHierarchyService<T> getRelationsHierarchy(Comparator<Tree<OWLNamedIndividual>> comparator);

     EntityType<T> getEntityType();

     @Override
     default List<Characteristic> getInferredCharacteristics(T entity, OWLReasoner reasoner) {
          return List.of(); // see instance service
     }
}
