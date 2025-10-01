package org.ontbrowser.www.feature.entities;

import org.apache.commons.lang3.NotImplementedException;
import org.ontbrowser.www.feature.entities.characteristics.CharacteristicsBuilder;
import org.ontbrowser.www.feature.entities.characteristics.DataPropertyCharacteristicsBuilder;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLDataPropertyHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLHierarchyService;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;

import static org.ontbrowser.www.model.Tree.treeComparator;

@Service
public class OWLDataPropertiesService implements PropertiesService<OWLDataProperty> {

    private final ReasonerFactoryService reasonerFactoryService;

    public OWLDataPropertiesService(ReasonerFactoryService reasonerFactoryService) {
        this.reasonerFactoryService = reasonerFactoryService;
    }

    public CharacteristicsBuilder<OWLDataProperty> getCharacteristicsBuilder(
            final OWLDataProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new DataPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize);
    }

    @Override
    public OWLHierarchyService<OWLDataProperty> getHierarchyService(OWLOntology ont) {
        return new OWLDataPropertyHierarchyService(
                reasonerFactoryService.getToldReasoner(ont),
                treeComparator());
    }

    @Override
    public Comparator<Tree<OWLNamedIndividual>> getComparator(OWLDataProperty orderByProperty, OWLOntology ont) {
        throw new NotImplementedException("Not implemented");
    }

    @Override
    public AbstractRelationsHierarchyService<OWLDataProperty> getRelationsHierarchy(Comparator<Tree<OWLNamedIndividual>> comparator) {
        throw new NotImplementedException("Not implemented");
    }

    @Override
    public EntityType<OWLDataProperty> getEntityType() {
        return EntityType.DATA_PROPERTY;
    }
}
