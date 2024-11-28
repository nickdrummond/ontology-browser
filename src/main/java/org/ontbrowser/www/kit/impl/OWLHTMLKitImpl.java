package org.ontbrowser.www.kit.impl;

import org.ontbrowser.www.renderer.*;
import org.ontbrowser.www.url.RestURLScheme;
import org.ontbrowser.www.url.URLScheme;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.ontbrowser.www.util.OWLObjectComparator;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.util.VocabUtils;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.StringWriter;
import java.net.URI;
import java.util.Comparator;
import java.util.Optional;
import java.util.Set;

public class OWLHTMLKitImpl implements OWLHTMLKit {

    private final OWLOntologyManager mngr;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider ontologySFP;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator comparator;

    private final OWLOntology rootOntology;

    protected URLScheme urlScheme;

    private URI labelURI;

    private String labelLang;

    private MOSStringRenderer stringRenderer;

    public OWLHTMLKitImpl(
            final OWLOntologyManager mngr,
            final OWLOntology rootOntology,
            final OntologyIRIShortFormProvider ontologySFP) {
        this.mngr = mngr;
        this.rootOntology = rootOntology;
        this.ontologySFP = ontologySFP;
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme();
        }
        return urlScheme;
    }

    @Override
    public void setLabelParams(URI labelURI, String labelLang) {
        this.labelURI = labelURI;
        this.labelLang = labelLang;
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

    @Override
    public String render(OWLObject owlObject) {
        return getStringRenderer().render(owlObject);
    }

    private MOSStringRenderer getStringRenderer() {
        if (stringRenderer == null) {
            stringRenderer = new MOSStringRenderer(getFinder(), rootOntology);
        }
        return stringRenderer;
    }

    public OntologyIRIShortFormProvider getOntologySFP() {
        return ontologySFP;
    }

    public OWLOntology getRootOntology() {
        return rootOntology;
    }

    private ShortFormProvider getInternalSFP() {
        if (shortFormProvider == null){
            OWLDataFactory df = mngr.getOWLDataFactory();
            OWLAnnotationProperty labelAnnotationProperty = df.getOWLAnnotationProperty(IRI.create(labelURI));
            if (VocabUtils.isSkosXLLabelAnnotation(labelAnnotationProperty)) {
                shortFormProvider = new SkosXLShortFormProvider(
                        labelLang, getOntologies(), new FixedSimpleShortFormProvider());
            }
            else {
                shortFormProvider = new LabelShortFormProvider(labelAnnotationProperty,
                        labelLang, getOntologies(), new FixedSimpleShortFormProvider());
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
