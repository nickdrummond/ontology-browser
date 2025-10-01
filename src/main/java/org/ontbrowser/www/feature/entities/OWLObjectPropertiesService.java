package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.CharacteristicsBuilder;
import org.ontbrowser.www.feature.entities.characteristics.ObjectPropertyCharacteristicsBuilder;
import org.ontbrowser.www.feature.hierarchy.*;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;

@Service
public class OWLObjectPropertiesService implements PropertiesService<OWLObjectProperty> {

    private final ReasonerFactoryService reasonerFactoryService;

    public OWLObjectPropertiesService(ReasonerFactoryService reasonerFactoryService) {
        this.reasonerFactoryService = reasonerFactoryService;
    }

    @Override
    public CharacteristicsBuilder<OWLObjectProperty> getCharacteristicsBuilder(
            final OWLObjectProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new ObjectPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize);
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

    public AbstractRelationsHierarchyService<OWLObjectProperty> getRelationsHierarchy(
            Comparator<Tree<OWLNamedIndividual>> comparator) {
        return new RelationsHierarchyService(comparator);
    }

    @Override
    public EntityType<OWLObjectProperty> getEntityType() {
        return EntityType.OBJECT_PROPERTY;
    }
}
