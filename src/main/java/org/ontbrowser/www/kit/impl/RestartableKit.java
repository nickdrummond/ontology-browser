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

import static org.ontbrowser.www.kit.impl.OWLHTMLKitInternals.editableKit;
import static org.ontbrowser.www.kit.impl.OWLHTMLKitInternals.readOnlyKit;

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
            if (getDelegate().isEditable()) {
                log.info("Restarting editable kit");
                delegate = editableKit(ont, config);
            } else {
                log.info("Restarting read-only kit");
                delegate = readOnlyKit(ont, config);
            }
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

    protected OWLHTMLKit getDelegate() {
        return delegate;
    }

    @Override
    public final Config getConfig() {
        return getDelegate().getConfig();
    }

    @Override
    public final URLScheme getURLScheme() {
        return getDelegate().getURLScheme();
    }

    @Override
    public final Set<OWLOntology> getOntologies() {
        return getDelegate().getOntologies();
    }

    @Override
    public final Optional<OWLOntology> getOntologyForIRI(IRI iri) {
        return getDelegate().getOntologyForIRI(iri);
    }

    @Override
    public final OWLOntologyManager getOWLOntologyManager() {
        return getDelegate().getOWLOntologyManager();
    }

    @Override
    public final Comparator<OWLObject> getComparator() {
        return getDelegate().getComparator();
    }

    @Override
    public final OWLEntityFinder getFinder() {
        return getDelegate().getFinder();
    }

    @Override
    public final OWLEntityChecker getOWLEntityChecker() {
        return getDelegate().getOWLEntityChecker();
    }

    @Override
    public final EntityIdLookup lookup() {
        return getDelegate().lookup();
    }

    @Override
    public final ShortFormProvider getShortFormProvider() {
        return getDelegate().getShortFormProvider();
    }

    @Override
    public final MOSStringRenderer getStringRenderer() {
        return getDelegate().getStringRenderer();
    }

    @Override
    public final OntologyIRIShortFormProvider getOntologySFP() {
        return getDelegate().getOntologySFP();
    }

    @Override
    public final OWLOntology getRootOntology() {
        return getDelegate().getRootOntology();
    }

    @Override
    public final Map<String, String> getPrefixes() {
        return getDelegate().getPrefixes();
    }

    @Override
    public final IRIShortFormProvider getIriShortFormProvider() {
        return getDelegate().getIriShortFormProvider();
    }

    @Override
    public boolean isEditable() {
        return getDelegate().isEditable();
    }
}
