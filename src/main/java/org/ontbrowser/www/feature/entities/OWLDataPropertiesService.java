package org.ontbrowser.www.feature.entities;

import org.apache.commons.lang3.NotImplementedException;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.DataPropertyCharacteristicsBuilder;
import org.ontbrowser.www.feature.hierarchy.AbstractRelationsHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLDataPropertyHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLHierarchyService;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.model.Tree;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
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

    public List<Characteristic> getCharacteristics(
            final OWLDataProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new DataPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize).getCharacteristics();
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
}
