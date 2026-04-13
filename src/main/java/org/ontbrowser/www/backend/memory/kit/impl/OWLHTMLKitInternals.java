package org.ontbrowser.www.backend.memory.kit.impl;

import org.ontbrowser.www.backend.EntityIdLookup;
import org.ontbrowser.www.backend.QNameEntityIdLookup;
import org.ontbrowser.www.backend.OWLEntityFinder;
import org.ontbrowser.www.backend.memory.kit.OWLHTMLKit;
import org.ontbrowser.www.configuration.Config;
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
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import java.util.*;

public class OWLHTMLKitInternals implements OWLHTMLKit {

    private final OWLOntologyManager mngr;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider ontologySFP;

    private PrefixIRIShortFormProvider prefixIRIProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator comparator;

    private final OWLOntology rootOntology;

    protected URLScheme urlScheme;

    private Config config;

    private MOSStringRenderer stringRenderer;
    private QNameEntityIdLookup entityIdLookup;
    private Map<String, String> prefixes;

    private final boolean editable;


    public static OWLHTMLKitInternals readOnlyKit(OWLOntology rootOntology, Config config) {
        return new OWLHTMLKitInternals(rootOntology, config, false);
    }

    public static OWLHTMLKitInternals editableKit(OWLOntology rootOntology, Config config) {
        return new OWLHTMLKitInternals(rootOntology, config, true);
    }

    private OWLHTMLKitInternals(
            final OWLOntology rootOntology,
            final Config config,
            final boolean editable
    ) {
        this.mngr = rootOntology.getOWLOntologyManager();
        this.rootOntology = rootOntology;
        this.config = config;
        this.editable = editable;
        ToStringRenderer.setRenderer(this::getStringRenderer);
    }

    @Override
    public Config getConfig() {
        return config;
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme(getIriShortFormProvider());
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

    @Override
    public EntityIdLookup lookup() {
        if (entityIdLookup == null){
            entityIdLookup = new QNameEntityIdLookup(getPrefixes());
        }
        return entityIdLookup;
    }

    public ShortFormProvider getShortFormProvider() {
        return getNameCache();
    }

    @Override
    public ShortFormProvider getShortFormProvider(OWLAnnotationProperty prop) {
        var noLabelSFP = new FixedSimpleShortFormProvider(getIriShortFormProvider());
        if (VocabUtils.isSkosXLLabelAnnotation(prop)) {
            return new SkosXLShortFormProvider(
                    config.labelLang(), getOntologies(), noLabelSFP);
        }
        else {
            return new LabelShortFormProvider(prop,
                    config.labelLang(), getOntologies(), noLabelSFP);
        }
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

    @Override
    public Map<String, String> getPrefixes() {
        if (prefixes == null) {
            prefixes = new HashMap<>();
            prefixes.put("swrlb", "http://www.w3.org/2003/11/swrlb#");
            // accumulate all prefixes
            for (var ont: rootOntology.getImportsClosure()) {
                var format = ont.getFormat();
                if (format != null && format.isPrefixOWLDocumentFormat()) {
                    format.asPrefixOWLDocumentFormat().getPrefixName2PrefixMap()
                            .forEach((key, value) -> prefixes.put(key.substring(0, key.length() - 1), value));
                }
            }
        }
        return prefixes;
    }

    @Override
    public IRIShortFormProvider getIriShortFormProvider() {
        if (prefixIRIProvider == null) {
            prefixIRIProvider = new PrefixIRIShortFormProvider(getPrefixes());
        }
        return prefixIRIProvider;
    }

    private ShortFormProvider getInternalSFP() {
        if (shortFormProvider == null){
            var df = mngr.getOWLDataFactory();
            var labelAnnotationProperty = df.getOWLAnnotationProperty(config.labelIRI());
            shortFormProvider = getShortFormProvider(labelAnnotationProperty);
        }
        return shortFormProvider;
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (nameCache == null){
            nameCache = new QuotingBiDirectionalShortFormProvider(getInternalSFP(), getOntologies());
        }
        return nameCache;
    }

    @Override
    public boolean isEditable() {
        return editable;
    }
}
