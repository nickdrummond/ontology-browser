package org.ontbrowser.www.feature.stats;

import org.apache.commons.collections4.map.LRUMap;
import org.ontbrowser.www.feature.hierarchy.OWLHierarchyService;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.event.RestartEvent;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;

import java.util.Set;

import static org.ontbrowser.www.feature.stats.StatsHelper.getBarData;
import static org.springframework.http.HttpStatus.NOT_FOUND;

@Service
public class StatsService {

    private static final Logger log = LoggerFactory.getLogger(StatsService.class);

    private final OWLHTMLKit kit;

    private final LRUMap<StatsMemo, Stats> cache = new LRUMap<>(20);
    private EntityCounts entityCountsTotal = null;

    public StatsService(RestartableKit kit) {
        this.kit = kit;
    }

    public Stats<OWLClass> getClassStats(String type, OWLReasoner reasoner) {
        return switch (type) {
            case ClassDescendantsStats.NAME -> new ClassDescendantsStats(reasoner);
            case InferredInstancesStats.NAME -> new InferredInstancesStats(reasoner);
            default -> throw new ResponseStatusException(NOT_FOUND, "No class stats called " + type);
        };
    }

    public Stats<OWLObjectProperty> getPropertyStats(String type, OWLReasoner reasoner) {
        return switch (type) {
            case RelationsCountStats.NAME -> cache.computeIfAbsent(
                    new StatsMemo(type, reasoner.getRootOntology().getOntologyID().toString()), m ->
                        new RelationsCountStats(reasoner)
                    );
            default -> throw new ResponseStatusException(NOT_FOUND, "No object property stats called " + type);
        };
    }

    public Stats<OWLAnnotationProperty> getAnnotationPropertyStats(
            String type, OWLOntology ont, OWLHierarchyService<OWLAnnotationProperty> hierarchyService) {
        return switch (type) {
            case AnnotationRelationsCountStats.NAME -> new AnnotationRelationsCountStats(ont, hierarchyService);
            default -> throw new ResponseStatusException(NOT_FOUND, "No annotation property stats called " + type);
        };
    }

    public Stats<OWLNamedIndividual> getIndividualStats(String type, OWLObjectProperty prop, boolean inverse, OWLReasoner reasoner) {
        return switch (type) {
            case TransitiveRelationsStats.NAME -> new TransitiveRelationsStats(reasoner, prop);
            default -> throw new ResponseStatusException(NOT_FOUND, "No individual stats called " + type);
        };
    }

    public <T extends OWLObject> Stats<T> getTreeStats(StatsMemo memo, OWLHierarchyService<T> hierarchyService) {
        return cache.computeIfAbsent(memo, m -> new TreeStats<>(hierarchyService));
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

    // TODO caching
    public OntologyStats getOntologyStats(OWLOntology ont, Imports imports) {
        return new OntologyStats(
                getCounts(ont, imports),
                getAxiomCounts(ont, imports),
                getAxiomsTypeCounts(ont, imports),
                getChildCountDistribution(ont, imports)
        );
    }

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
        int annotationCount = ont.getAxioms(imports).size() - logicalAxiomCount - declarationCount;
        return new AxiomCounts(logicalAxiomCount, declarationCount, annotationCount);
    }

    public EntityCounts getCounts(OWLOntology ont, Imports imports) {
        return new EntityCounts(
                ont.getClassesInSignature(imports).size(),
                ont.getIndividualsInSignature(imports).size(),
                ont.getObjectPropertiesInSignature(imports).size(),
                ont.getDataPropertiesInSignature(imports).size(),
                ont.getAnnotationPropertiesInSignature(imports).size(),
                ont.getDatatypesInSignature(imports).size()
        );
    }

    public EntityCounts getEntityCountsTotal() {
        if (entityCountsTotal == null) {
            this.entityCountsTotal = getCounts(kit.getRootOntology(), Imports.INCLUDED);
        }
        return entityCountsTotal;
    }

    @EventListener
    public void onRestart(RestartEvent event) {
        log.info("Clearing stats cache");
        cache.clear();
        entityCountsTotal = null;
    }
}
