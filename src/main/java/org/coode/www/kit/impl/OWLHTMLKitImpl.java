package org.coode.www.kit.impl;

import org.coode.html.url.RestURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.impl.OWLEntityFinderImpl;
import org.coode.owl.mngr.impl.OWLObjectComparator;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.FixedSimpleShortFormProvider;
import org.coode.www.renderer.LabelShortFormProvider;
import org.coode.www.renderer.OntologyShortFormProvider;
import org.coode.www.renderer.QuotingBiDirectionalShortFormProvider;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import javax.annotation.Nonnull;
import java.net.URI;
import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

public class OWLHTMLKitImpl implements OWLHTMLKit {

    private final OWLOntologyManager mngr;

    private OWLOntology activeOntology;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider uriShortFormProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator comparator;

    private final OWLOntology rootOntology;

    protected URLScheme urlScheme;

    private URI labelURI;

    private String labelLang;

    public OWLHTMLKitImpl(final OWLOntologyManager mngr, final OWLOntology rootOntology) {
        this.mngr = mngr;
        this.rootOntology = rootOntology;
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme(this);
        }
        return urlScheme;
    }

    @Override
    public void setLabelParams(URI labelURI, String labelLang) {
        this.labelURI = labelURI;
        this.labelLang = labelLang;
    }

    public Optional<OWLOntology> getOntologyForIRI(final IRI iri) {
        return mngr.getOntologies().stream().filter(o -> {
                    OWLOntologyID id = o.getOntologyID();
                    return ((id.getVersionIRI().isPresent() && id.getVersionIRI().get().equals(iri)) ||
                            (id.getOntologyIRI().isPresent() && id.getOntologyIRI().get().equals(iri)) ||
                            (mngr.getOntologyDocumentIRI(o).equals(iri)));
                })
                .findFirst();
    }

    public OWLOntology getActiveOntology() {
        if (activeOntology == null){
            activeOntology = rootOntology;
        }
        return activeOntology;
    }

    public void setActiveOntology(@Nonnull OWLOntology ont) {
        activeOntology = ont;
    }

    public Set<OWLOntology> getOntologies() {
        return mngr.getOntologies();
    }

    public Set<OWLOntology> getActiveOntologies() {
        return mngr.getImportsClosure(getActiveOntology());
    }

    public OWLOntologyManager getOWLOntologyManager() {
        return mngr;
    }

    public Comparator<OWLObject> getComparator() {
        if (comparator == null){
            comparator = new OWLObjectComparator(getShortFormProvider());
        }
        return comparator;
    }

    public OWLEntityFinder getFinder() {
        if (finder == null){
            finder = new OWLEntityFinderImpl(getNameCache(), getOWLOntologyManager().getOWLDataFactory(), this);
        }
        return finder;
    }


    public OWLEntityChecker getOWLEntityChecker() {
        if (owlEntityChecker == null){
            owlEntityChecker = new ShortFormEntityChecker(getNameCache());
        }
        return owlEntityChecker;
    }

    public ShortFormProvider getShortFormProvider() {
        if (shortFormProvider == null){
            OWLDataFactory df = mngr.getOWLDataFactory();
            OWLAnnotationProperty labelAnnotationProperty = df.getOWLAnnotationProperty(IRI.create(labelURI));
            shortFormProvider = new LabelShortFormProvider(labelAnnotationProperty,
                    labelLang,
                    getActiveOntologies(),
                    new FixedSimpleShortFormProvider());
        }
        return shortFormProvider;
    }


    public OntologyIRIShortFormProvider getOntologyShortFormProvider() {
        if (uriShortFormProvider == null){
            uriShortFormProvider = new OntologyShortFormProvider(getRootOntology());
        }
        return uriShortFormProvider;
    }

    public OWLOntology getRootOntology() {
        return rootOntology;
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (nameCache == null){
            nameCache = new QuotingBiDirectionalShortFormProvider(getShortFormProvider(), getActiveOntologies());
        }
        return nameCache;
    }
}
