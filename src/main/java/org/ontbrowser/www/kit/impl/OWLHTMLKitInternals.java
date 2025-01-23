package org.ontbrowser.www.kit.impl;

import org.ontbrowser.www.kit.Config;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.renderer.*;
import org.ontbrowser.www.url.RestURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.ontbrowser.www.util.OWLObjectComparator;
import org.ontbrowser.www.util.VocabUtils;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.io.ToStringRenderer;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

public class OWLHTMLKitInternals implements OWLHTMLKit {

    private final OWLOntologyManager mngr;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider ontologySFP;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator comparator;

    private final OWLOntology rootOntology;

    protected URLScheme urlScheme;

    private Config config;

    private MOSStringRenderer stringRenderer;

    public OWLHTMLKitInternals(
            final OWLOntology rootOntology,
            final Config config
    ) {
        this.mngr = rootOntology.getOWLOntologyManager();
        this.rootOntology = rootOntology;
        this.config = config;
        ToStringRenderer.setRenderer(this::getStringRenderer);
    }

    @Override
    public void restart() {
        throw new RuntimeException("Cannot restart the internal implementation");
    }

    @Override
    public Config getConfig() {
        return config;
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme();
        }
        return urlScheme;
    }

    public Optional<OWLOntology> getOntologyForIRI(final IRI iri) {
        return mngr.getOntologies().stream().filter(ont -> {
                    OWLOntologyID id = ont.getOntologyID();
                    return (id.getVersionIRI().filter(v -> v.equals(iri)).isPresent() ||
                            id.getOntologyIRI().filter(o -> o.equals(iri)).isPresent() ||
                            mngr.getOntologyDocumentIRI(ont).equals(iri));
                })
                .findFirst();
    }

    public Set<OWLOntology> getOntologies() {
        return mngr.getOntologies();
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
            finder = new OWLEntityFinderImpl(getNameCache(), getOWLOntologyManager().getOWLDataFactory());
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
        return getNameCache();
    }

    public MOSStringRenderer getStringRenderer() {
        if (stringRenderer == null) {
            stringRenderer = new MOSStringRenderer(getFinder(), rootOntology);
        }
        return stringRenderer;
    }

    public OntologyIRIShortFormProvider getOntologySFP() {
        if (ontologySFP == null){
            ontologySFP = new OntologyShortFormProvider(OWLRDFVocabulary.RDFS_LABEL.getIRI());
        }
        return ontologySFP;
    }

    public OWLOntology getRootOntology() {
        return rootOntology;
    }

    private ShortFormProvider getInternalSFP() {
        if (shortFormProvider == null){
            OWLDataFactory df = mngr.getOWLDataFactory();
            OWLAnnotationProperty labelAnnotationProperty = df.getOWLAnnotationProperty(config.labelIRI());
            if (VocabUtils.isSkosXLLabelAnnotation(labelAnnotationProperty)) {
                shortFormProvider = new SkosXLShortFormProvider(
                        config.labelLang(), getOntologies(), new FixedSimpleShortFormProvider());
            }
            else {
                shortFormProvider = new LabelShortFormProvider(labelAnnotationProperty,
                        config.labelLang(), getOntologies(), new FixedSimpleShortFormProvider());
            }
        }
        return shortFormProvider;
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (nameCache == null){
            nameCache = new QuotingBiDirectionalShortFormProvider(getInternalSFP(), getOntologies());
        }
        return nameCache;
    }
}
