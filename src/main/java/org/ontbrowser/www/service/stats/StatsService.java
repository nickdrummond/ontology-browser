package org.ontbrowser.www.service.stats;

import org.apache.commons.collections4.map.LRUMap;
import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.RestartListener;
import org.ontbrowser.www.service.hierarchy.OWLHierarchyService;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

@Service
public class StatsService implements RestartListener {

    private static final Logger log = LoggerFactory.getLogger(StatsService.class);

    private final LRUMap<StatsMemo, Stats> cache = new LRUMap<>(20);

    public StatsService(OWLHTMLKit kit) {
        kit.registerListener(this);
    }

    public Stats<OWLClass> getClassStats(String type, OWLReasoner reasoner) throws NotFoundException {
        return switch (type) {
            case ClassDescendantsStats.NAME -> new ClassDescendantsStats(reasoner);
            case InferredInstancesStats.NAME -> new InferredInstancesStats(reasoner);
            default -> throw new NotFoundException("No class stats called " + type);
        };
    }

    public Stats<OWLObjectProperty> getPropertyStats(String type, OWLReasoner reasoner) throws NotFoundException {
        return switch (type) {
            case RelationsCountStats.NAME -> cache.computeIfAbsent(
                    new StatsMemo(type, reasoner.getRootOntology().getOntologyID().toString()), m ->
                        new RelationsCountStats(reasoner)
                    );
            default -> throw new NotFoundException("No object property stats called " + type);
        };
    }

    public Stats<OWLAnnotationProperty> getAnnotationPropertyStats(
            String type, OWLOntology ont, OWLHierarchyService<OWLAnnotationProperty> hierarchyService) throws NotFoundException {
        return switch (type) {
            case AnnotationRelationsCountStats.NAME -> new AnnotationRelationsCountStats(ont, hierarchyService);
            default -> throw new NotFoundException("No annotation property stats called " + type);
        };
    }

    public Stats<OWLNamedIndividual> getIndividualStats(String type, OWLObjectProperty prop, boolean inverse, OWLReasoner reasoner) throws NotFoundException {
        return switch (type) {
            case TransitiveRelationsStats.NAME -> new TransitiveRelationsStats(reasoner, prop);
            default -> throw new NotFoundException("No individual stats called " + type);
        };
    }

    public <T extends OWLObject> Stats<T> getTreeStats(StatsMemo memo, OWLHierarchyService<T> hierarchyService) {
        return cache.computeIfAbsent(memo, m -> new TreeStats<>(hierarchyService));
    }

    @Override
    public void onRestart() {
        log.info("Clearing stats cache");
        cache.clear();
    }
}
