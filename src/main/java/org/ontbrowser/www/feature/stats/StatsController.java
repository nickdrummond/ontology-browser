package org.ontbrowser.www.feature.stats;

import org.ontbrowser.www.controller.ApplicationController;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.web.bind.annotation.*;

import java.util.Set;

import static org.ontbrowser.www.feature.stats.StatsHelper.getBarData;

@RestController
@RequestMapping(value = "/stats")
public class StatsController extends ApplicationController {

    @GetMapping()
    public OntologyStats getStats(
            @ModelAttribute final OWLOntology ont,
            @RequestParam(required = false, defaultValue = "EXCLUDED") final Imports imports
    ) {
        return new OntologyStats(
                getCounts(ont, imports),
                getAxiomCounts(ont, imports),
                getAxiomsTypeCounts(ont, imports),
                getChildCountDistribution(ont, imports)
        );
    }

    private AxiomTypeCounts getAxiomsTypeCounts(OWLOntology ont, Imports imports) {
        var counts = new AxiomTypeCounts();
        AxiomType.AXIOM_TYPES.stream()
                .filter(axiomType ->
                        axiomType != AxiomType.ANNOTATION_ASSERTION &&
                        axiomType != AxiomType.DECLARATION)
                .forEach(axiomType ->
                        counts.put(axiomType, ont.getAxiomCount(axiomType, imports))
                );
        return counts;
    }

    // TODO use StatsService
    // TODO caching
    private Set<Coords<Long>> getChildCountDistribution(OWLOntology ont, Imports imports) {
        return getBarData(ont.classesInSignature(imports), cls -> getSubclassCount(cls, ont, imports));
    }

    private Long getSubclassCount(OWLClass cls, OWLOntology ont, Imports imports) {
        return ont.referencingAxioms(cls, imports)
                .filter(OWLSubClassOfAxiom.class::isInstance)
                .filter(ax -> ((OWLSubClassOfAxiom) ax).getSuperClass().equals(cls))
                .count();
    }

    private AxiomCounts getAxiomCounts(OWLOntology ont, Imports imports) {
        int logicalAxiomCount = ont.getLogicalAxioms(imports).size();
        int declarationCount = ont.getAxiomCount(AxiomType.DECLARATION, imports);
        int annotationCOunt = ont.getAxioms(imports).size() - logicalAxiomCount - declarationCount;
        return new AxiomCounts(logicalAxiomCount, declarationCount, annotationCOunt);
    }

    private EntityCounts getCounts(OWLOntology ont, Imports imports) {
        return new EntityCounts(
                ont.getClassesInSignature(imports).size(),
                ont.getIndividualsInSignature(imports).size(),
                ont.getObjectPropertiesInSignature(imports).size(),
                ont.getDataPropertiesInSignature(imports).size(),
                ont.getAnnotationPropertiesInSignature(imports).size(),
                ont.getDatatypesInSignature(imports).size()
        );
    }
}
