package org.ontbrowser.www.service;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;

public interface PropertiesService<T extends OWLProperty> {

     T getPropertyFor(String propertyId, OWLOntology ont) throws NotFoundException;

     String getIdFor(final T property);

     List<Characteristic> getCharacteristics(
             final T property,
             final OWLOntology ont,
             final Comparator<OWLObject> comparator,
             final List<With> with,
             final int pageSize);

     Comparator<Tree<OWLNamedIndividual>> getComparator(T orderByProperty, OWLOntology ont);

     Tree<? extends OWLObject> getPropTree(T property, OWLOntology ont);

     AbstractRelationsHierarchyService<T> getRelationsHierarchy (Comparator<Tree<OWLNamedIndividual>> comparator);
}
