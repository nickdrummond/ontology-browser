package org.ontbrowser.www.service.stats;

import org.ontbrowser.www.service.hierarchy.OWLAnnotationPropertyHierarchyService;
import org.ontbrowser.www.service.hierarchy.OWLHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;

class AnnotationRelationsCountStats implements Stats<OWLAnnotationProperty> {

    public static final String NAME = "annotationsCount";

    // TODO cache

    private OWLOntology ont;
    private final OWLHierarchyService<OWLAnnotationProperty> hierarchyService;

    public AnnotationRelationsCountStats(OWLOntology ont, OWLHierarchyService<OWLAnnotationProperty> hierarchyService) {
        this.ont = ont;
        this.hierarchyService = hierarchyService;
    }

    public int getStats(OWLAnnotationProperty prop) {
        int directRelations = getNumberOfRelations(prop);
        return directRelations + hierarchyService.getDescendants(prop)
                .map(p -> getNumberOfRelations(p.getRepresentativeElement())).mapToInt(i -> i).sum();
    }

    @Override
    public String getName() {
        return NAME;
    }

    private int getNumberOfRelations(OWLAnnotationProperty prop) {
        return ont.axioms(AxiomType.ANNOTATION_ASSERTION, Imports.INCLUDED)
                .filter(ax -> ax.getProperty().equals(prop)).toList().size();
    }
}
