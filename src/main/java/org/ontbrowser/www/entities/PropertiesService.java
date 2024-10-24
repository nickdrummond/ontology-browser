package org.ontbrowser.www.entities;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.entities.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLHierarchyService;
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

     OWLHierarchyService<? super T> getHierarchyService(OWLOntology ont);

     AbstractRelationsHierarchyService<T> getRelationsHierarchy (Comparator<Tree<OWLNamedIndividual>> comparator);
}
