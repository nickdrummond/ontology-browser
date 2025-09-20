package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLHierarchyService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;

public interface PropertiesService<T extends OWLProperty> {

     List<Characteristic> getCharacteristics(
             final T property,
             final OWLOntology ont,
             final Comparator<OWLObject> comparator,
             final List<With> with,
             final int pageSize);

     Comparator<Tree<OWLNamedIndividual>> getComparator(T orderByProperty, OWLOntology ont);

     OWLHierarchyService<? super T> getHierarchyService(OWLOntology ont);

     AbstractRelationsHierarchyService<T> getRelationsHierarchy(Comparator<Tree<OWLNamedIndividual>> comparator);

     EntityType<T> getEntityType();
}
