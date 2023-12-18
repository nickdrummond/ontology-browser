package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Tree;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.service.hierarchy.AbstractRelationsHierarchyService;
import org.semanticweb.owlapi.model.*;

import java.util.Comparator;
import java.util.List;

public interface PropertiesService<T extends OWLProperty> {

     T getPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException;

     String getIdFor(final T property);

     List<Characteristic> getCharacteristics(final T property, final OWLHTMLKit kit);

     Comparator<Tree<OWLNamedIndividual>> getComparator(T orderByProperty, OWLOntology ont);

     Tree<? extends OWLObject> getPropTree(T property, OWLOntology ont);

     AbstractRelationsHierarchyService<T> getRelationsHierarchy (Comparator<Tree<OWLNamedIndividual>> comparator);

    boolean isEquivalentOrSubproperty(OWLObjectPropertyExpression property, OWLObjectProperty superProperty, OWLOntology ont);
}
