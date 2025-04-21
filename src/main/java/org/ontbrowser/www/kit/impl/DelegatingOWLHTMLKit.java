package org.ontbrowser.www.kit.impl;

import org.ontbrowser.www.io.OntologyLoader;
import org.ontbrowser.www.kit.Config;
import org.ontbrowser.www.kit.RestartListener;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.ontbrowser.www.service.EntityIdLookup;
import org.ontbrowser.www.url.URLScheme;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.*;

// Delegate pattern allows the implementation to be switched out
public class DelegatingOWLHTMLKit implements OWLHTMLKit {

    private OWLHTMLKit delegate;

    private final List<RestartListener> listeners = new ArrayList<>();

    public DelegatingOWLHTMLKit(OWLHTMLKit delegate) {
        this.delegate = delegate;
    }

    @Override
    public void restart() throws OWLOntologyCreationException {
        var config = getConfig();
        var ont = new OntologyLoader().loadOntologies(config.root());
        // only switches the implementation if the loading was successful
        delegate = new OWLHTMLKitInternals(ont, config);
        listeners.forEach(RestartListener::onRestart);
    }

    @Override
    public void registerListener(RestartListener listener) {
        listeners.add(listener);
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
}
