package org.coode.www.kit.impl;

import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import org.coode.html.url.RestURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.impl.*;
import org.coode.owl.mngr.impl.OWLObjectComparator;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.renderer.FixedSimpleShortFormProvider;
import org.coode.www.renderer.OntologyShortFormProvider;
import org.coode.www.renderer.QuotingBiDirectionalShortFormProvider;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import java.net.URI;
import java.util.*;
import java.util.Optional;

public class OWLHTMLKitImpl implements OWLHTMLKit {

    private static final Logger logger = LoggerFactory.getLogger(OWLHTMLKitImpl.class);

    // TODO move to config
    private static final String RENDERER_LABEL = "label";

    private final OWLOntologyManager mngr;

    private final URI root;

    private OWLOntology activeOntology;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider uriShortFormProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator comparator;

    private Map<URI, OWLOntologyIRIMapper> baseMapper = Maps.newHashMap();

    private final Set<ActiveOntologyProvider.Listener> listeners = Sets.newHashSet();

    private OWLOntology rootOntology;

    protected URLScheme urlScheme;

    public OWLHTMLKitImpl(OWLOntologyManager mngr, URI root) throws OWLOntologyCreationException {

        this.mngr = mngr;

        this.root = root;

        mngr.addOntologyLoaderListener(ontLoadListener);

        loadOntology(root);
    }

    @Override
    public void refresh() throws OWLOntologyCreationException {
        clear();
        loadOntology(root);
    }

    private void clear() {
        if (shortFormProvider != null){
            shortFormProvider.dispose();
            shortFormProvider = null;
        }
        if (finder != null){
            finder.dispose();
            finder = null;
        }
        if (nameCache != null){
            nameCache.dispose();
            nameCache = null;
        }
        uriShortFormProvider = null;
        owlEntityChecker = null;
        comparator = null;
        urlScheme = null;
        activeOntology = null;
        rootOntology = null;
        baseMapper.clear();
        mngr.getIRIMappers().clear();
        clearOntologies(); // as mngr.clearOntologies does not work??
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme(this);
        }
        return urlScheme;
    }

    private OWLOntologyLoaderListener ontLoadListener = new OWLOntologyLoaderListener() {
        public void startedLoadingOntology(LoadingStartedEvent loadingStartedEvent) {
            // do nothing
        }

        public void finishedLoadingOntology(LoadingFinishedEvent loadingFinishedEvent) {
            if (loadingFinishedEvent.isSuccessful() && !loadingFinishedEvent.isImported()){
                OWLOntologyID id = loadingFinishedEvent.getOntologyID();
                Optional<OWLOntology> ont = Optional.ofNullable(mngr.getOntology(id));
                if (ont.isPresent()){
                    ont = getOntologyForIRI(loadingFinishedEvent.getDocumentIRI());
                }
                ont.ifPresent(OWLHTMLKitImpl.this::loadedOntology);
            }
        }
    };

    public OWLOntology loadOntology(URI physicalURI) throws OWLOntologyCreationException {
        IRI iri = IRI.create(physicalURI);
        for (OWLOntology ont : getOntologies()){
            if (mngr.getOntologyDocumentIRI(ont).equals(iri)){
                return ont;
            }
        }

        handleCommonBaseMappers(physicalURI);

        return mngr.loadOntologyFromOntologyDocument(iri);
    }

    public void clearOntologies() {
        for (OWLOntology ont : mngr.getOntologies()){
            mngr.removeOntology(ont);
        }
    }

    private void loadedOntology(OWLOntology ont) {
        if (ont != null){
            logger.info("loaded " + getOntologyIdString(ont));
            rootOntology = ont;
            setActiveOntology(ont);
        }
        else {
            throw (new RuntimeException("Could not load ontology"));
        }
    }

    public Optional<OWLOntology> getOntologyForIRI(IRI iri) {
        for (OWLOntology ontology : getOntologies()){ // TODO make this better
            Optional<IRI> versionIRI = ontology.getOntologyID().getVersionIRI();
            if (versionIRI.isPresent()) {
                if (iri.equals(versionIRI.get())) {
                    return Optional.of(ontology);
                }
            }
        }
        for (OWLOntology ontology : getOntologies()) {
            Optional<IRI> ontologyIRI = ontology.getOntologyID().getOntologyIRI();
            if (ontologyIRI.isPresent()) {
                if (iri.equals(ontologyIRI.get())) {
                    return Optional.of(ontology);
                }
            }
        }

        // look for an ontology with this location
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(getOWLOntologyManager().getOntologyDocumentIRI(ontology))){
                return Optional.of(ontology);
            }
        }
        for (OWLOntology ontology : getOntologies()){
            if (iri.toString().equals(ontology.getOntologyID().toString())){
                return Optional.of(ontology);
            }
        }
        return Optional.empty();
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

    public void addActiveOntologyListener(ActiveOntologyProvider.Listener l) {
        listeners.add(l);
    }

    public void removeActiveOntologyListener(ActiveOntologyProvider.Listener l) {
        listeners.add(l);
    }

    private String getOntologyIdString(final OWLOntology ont){
        return ont.getOntologyID().getDefaultDocumentIRI().map(IRI::toString)
                .orElse(ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString());
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
            shortFormProvider = new FixedSimpleShortFormProvider();
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

    private Optional<IRI> getImportIRIForOntology(OWLOntology root) {
        if (root.isAnonymous()){
            // TODO need a workaround as this will not work
            // see OWL API bug - https://sourceforge.net/tracker/?func=detail&aid=3110834&group_id=90989&atid=595534
            return Optional.ofNullable(mngr.getOntologyDocumentIRI(root));
        }
        return root.getOntologyID().getDefaultDocumentIRI();
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (nameCache == null){
            // TODO throw away if the onts or the provider change
            nameCache = new QuotingBiDirectionalShortFormProvider(getShortFormProvider(), getActiveOntologies());
        }
        return nameCache;
    }

    // create a set of CommonBaseURIMappers for finding ontologies
    // using the base of explicitly loaded ontologies as a hint
    private void handleCommonBaseMappers(URI physicalURI) {
        String baseURIStr = "";
        String uriParts[] = physicalURI.toString().split("/");
        for (int i=0; i<uriParts.length-1; i++){
            baseURIStr += uriParts[i] + "/";
        }
        URI baseURI = URI.create(baseURIStr);

        if (baseMapper.get(baseURI) == null){
            final BaseURIMapper mapper = new BaseURIMapper(baseURI);
            baseMapper.put(baseURI, mapper);
            mngr.addIRIMapper(mapper);
        }
    }
}
