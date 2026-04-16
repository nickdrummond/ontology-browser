package org.ontbrowser.www.backend;

import org.ontbrowser.www.controller.AppStatus;
import org.ontbrowser.www.feature.cloud.model.CloudModel;
import org.ontbrowser.www.feature.cloud.model.CloudModelFactory;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.url.RestURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.Comparator;

public interface BackendContext {
    OWLOntology getRootOntology();

    OWLOntology getOntologyFor(String ontId);

    OntologyIRIShortFormProvider getOntologySFP();

    ShortFormProvider getShortFormProvider();

    ShortFormProvider getShortFormProvider(OWLAnnotationProperty prop);

    OWLDataFactory getOWLDataFactory();

    OWLReasoner getToldReasoner(OWLOntology ont);

    EntityIdLookup lookup();

    IRIShortFormProvider getIriShortFormProvider();

    Comparator<OWLObject> getComparator();

    OWLEntityFinder getFinder();

    OWLEntityChecker getOWLEntityChecker();

    AppStatus getStatus();

    StatsService getStats();

    /**
     * Returns a usage-frequency cloud for all entities of the given type in {@code ont}.
     * The default implementation uses the OWL API and is efficient for the in-memory backend.
     * Backends with direct data-source access (e.g. DB) should override this to fetch
     * entities and their usage counts in a single query and return
     * {@link CloudModelFactory#fromPreloadedValues(java.util.Map)}.
     */
    default <T extends OWLEntity> CloudModel<T> getUsageCloud(
            EntityType<T> entityType, OWLOntology ont, Imports imports) {
        return CloudModelFactory.getUsageCloud(entityType, ont, imports);
    }

    default URLScheme getURLScheme(){
        return new RestURLScheme(getIriShortFormProvider());
    };
}
