package org.ontbrowser.www.feature.stats;

import org.ontbrowser.www.controller.ApplicationController;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Set;

import static org.ontbrowser.www.feature.stats.StatsHelper.getBarData;

@RestController
@RequestMapping(value = "/stats")
public class StatsController extends ApplicationController {

    @GetMapping()
    public OntologyStats getStats() {
        var ont = kit.getRootOntology();
        return new OntologyStats(
                getCounts(ont),
                getAxiomCounts(ont),
                getAxiomsTypeCounts(ont),
                getChildCountDistribution(ont)
        );
    }

    private AxiomTypeCounts getAxiomsTypeCounts(OWLOntology ont) {
        var counts = new AxiomTypeCounts();
        AxiomType.AXIOM_TYPES.stream()
                .filter(axiomType ->
                        axiomType != AxiomType.ANNOTATION_ASSERTION &&
                        axiomType != AxiomType.DECLARATION)
                .forEach(axiomType ->
                        counts.put(axiomType, ont.getAxiomCount(axiomType, Imports.INCLUDED))
                );
        return counts;
    }

    // TODO use StatsService
    // TODO caching
    private Set<Coords<Long>> getChildCountDistribution(OWLOntology ont) {
        return getBarData(ont.classesInSignature(Imports.INCLUDED), cls -> getSubclassCount(cls, ont));
    }

    private Long getSubclassCount(OWLClass cls, OWLOntology ont) {
        return ont.referencingAxioms(cls, Imports.INCLUDED)
                .filter(OWLSubClassOfAxiom.class::isInstance)
                .filter(ax -> ((OWLSubClassOfAxiom) ax).getSuperClass().equals(cls))
                .count();
    }

    private AxiomCounts getAxiomCounts(OWLOntology ont) {
        int logicalAxiomCount = ont.getLogicalAxioms(Imports.INCLUDED).size();
        int declarationCount = ont.getAxiomCount(AxiomType.DECLARATION, Imports.INCLUDED);
        return new AxiomCounts(
                logicalAxiomCount,
                declarationCount,
                ont.getAxioms(Imports.INCLUDED).size() - logicalAxiomCount - declarationCount
        );
    }

    private EntityCounts getCounts(OWLOntology ont) {
        return new EntityCounts(
                ont.getClassesInSignature(Imports.INCLUDED).size(),
                ont.getIndividualsInSignature(Imports.INCLUDED).size(),
                ont.getObjectPropertiesInSignature(Imports.INCLUDED).size(),
                ont.getDataPropertiesInSignature(Imports.INCLUDED).size(),
                ont.getAnnotationPropertiesInSignature(Imports.INCLUDED).size(),
                ont.getDatatypesInSignature(Imports.INCLUDED).size()
        );
    }
}
