package org.ontbrowser.www.kit.impl;

import org.ontbrowser.www.BeforeLoad;
import org.ontbrowser.www.controller.AppStatus;
import org.ontbrowser.www.io.OntologyLoader;
import org.ontbrowser.www.kit.*;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.ontbrowser.www.kit.event.RestartEvent;

import java.util.*;

// Delegate pattern allows the implementation to be switched out
public class RestartableKit implements OWLHTMLKit, Restartable {

    private static final Logger log = LoggerFactory.getLogger(RestartableKit.class);

    private OWLHTMLKit delegate;

    private final List<BeforeLoad> beforeLoad;
    private AppStatus status = new AppStatus(AppStatus.Status.STARTING);
    private ApplicationEventPublisher eventPublisher;

    public RestartableKit(OWLHTMLKit delegate, List<BeforeLoad> beforeLoad, ApplicationEventPublisher eventPublisher) {
        this.delegate = delegate;
        this.beforeLoad = beforeLoad;
        this.eventPublisher = eventPublisher;
        status = new AppStatus(AppStatus.Status.UP);
    }

    @Override
    public void restart(Config config) {
        status = new AppStatus(AppStatus.Status.STARTING);
        beforeLoad.forEach(bl -> {
            log.info("Before load: {}", bl.getClass().getSimpleName());
            bl.beforeLoad(config);
        });
        try {
            OWLOntology ont = new OntologyLoader().loadOntologies(config.root());
            // only switches the implementation if the loading was successful
            delegate = new OWLHTMLKitInternals(ont, config);
            if (eventPublisher != null) {
                eventPublisher.publishEvent(new RestartEvent(this));
            }
            status = new AppStatus(AppStatus.Status.UP);
        } catch (OWLOntologyCreationException e) {
            log.error("Failed to restart", e);
        }
    }

    @Override
    public void restart() {
        restart(getConfig());
    }

    @Override
    public AppStatus getStatus() {
        return status;
    }

    @Override
    public Config getConfig() {
        return delegate.getConfig();
    }

    @Override
    public URLScheme getURLScheme() {
        return delegate.getURLScheme();
    }

    @Override
    public Set<OWLOntology> getOntologies() {
        return delegate.getOntologies();
    }

    @Override
    public Optional<OWLOntology> getOntologyForIRI(IRI iri) {
        return delegate.getOntologyForIRI(iri);
    }

    @Override
    public OWLOntologyManager getOWLOntologyManager() {
        return delegate.getOWLOntologyManager();
    }

    @Override
    public Comparator<OWLObject> getComparator() {
        return delegate.getComparator();
    }

    @Override
    public OWLEntityFinder getFinder() {
        return delegate.getFinder();
    }

    @Override
    public OWLEntityChecker getOWLEntityChecker() {
        return delegate.getOWLEntityChecker();
    }

    @Override
    public EntityIdLookup lookup() {
        return delegate.lookup();
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        return delegate.getShortFormProvider();
    }

    @Override
    public MOSStringRenderer getStringRenderer() {
        return delegate.getStringRenderer();
    }

    @Override
    public OntologyIRIShortFormProvider getOntologySFP() {
        return delegate.getOntologySFP();
    }

    @Override
    public OWLOntology getRootOntology() {
        return delegate.getRootOntology();
    }

    @Override
    public Map<String, String> getPrefixes() {
        return delegate.getPrefixes();
    }

    @Override
    public IRIShortFormProvider getIriShortFormProvider() {
        return delegate.getIriShortFormProvider();
    }
}
