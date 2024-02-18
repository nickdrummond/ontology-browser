package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.paging.With;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
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
