package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.ClassCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.feature.reasoner.ReasonerFactoryService;
import org.ontbrowser.www.feature.hierarchy.OWLClassHierarchyService;
import org.ontbrowser.www.feature.hierarchy.OWLHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.ontbrowser.www.model.Tree.treeComparator;
import static org.semanticweb.owlapi.model.AxiomType.SUBCLASS_OF;

@Service
public class OWLClassesService {

    private final ReasonerFactoryService reasonerFactoryService;

    public OWLClassesService(ReasonerFactoryService reasonerFactoryService) {
        this.reasonerFactoryService = reasonerFactoryService;
    }

    public OWLHierarchyService<OWLClass> getHierarchyService(OWLOntology ont) {
        OWLReasoner r = reasonerFactoryService.getToldReasoner(ont);
        return new OWLClassHierarchyService(r, treeComparator());
    }

    public List<Characteristic> getCharacteristics(
            final OWLClass owlClass,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int defaultPageSize) {
        return new ClassCharacteristicsBuilder(owlClass, ont, comparator, with, defaultPageSize).getCharacteristics();
    }

    public Optional<Characteristic> getCharacteristic(
            final String name,
            final OWLClass owlClass,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int defaultPageSize) {
        return new ClassCharacteristicsBuilder(owlClass, ont, comparator, with, defaultPageSize).getCharacteristic(name);
    }

    public Set<OWLClass> getNamedTypes(OWLClass cls, OWLOntology ont) {
        return ont.getAxioms(cls, Imports.INCLUDED).stream()
                .filter(ax -> ax.isOfType(SUBCLASS_OF))
                .map(ax -> ((OWLSubClassOfAxiom)ax).getSuperClass())
                .filter(OWLClassExpression::isNamed)
                .map(OWLClassExpression::asOWLClass)
                .collect(Collectors.toSet());
    }
}
