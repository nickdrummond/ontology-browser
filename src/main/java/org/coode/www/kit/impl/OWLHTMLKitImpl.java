package org.coode.www.kit.impl;

import com.google.common.base.*;
import com.google.common.collect.Maps;
import com.google.common.collect.Sets;
import org.coode.html.url.RestURLScheme;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.owl.mngr.OWLReasonerManager;
import org.coode.owl.mngr.impl.*;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.OntologyMapping;
import org.coode.www.model.ServerConfig;
import org.coode.www.renderer.FixedSimpleShortFormProvider;
import org.coode.www.renderer.LabelShortFormProvider;
import org.coode.www.renderer.OntologyShortFormProvider;
import org.coode.www.renderer.QuotingBiDirectionalShortFormProvider;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.CachingBidirectionalShortFormProvider;
import org.semanticweb.owlapi.util.NonMappingOntologyIRIMapper;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.net.URI;
import java.net.URL;
import java.util.*;
import java.util.Optional;

public class OWLHTMLKitImpl implements OWLHTMLKit {

    private static final Logger logger = LoggerFactory.getLogger(OWLHTMLKitImpl.class);

    // TODO move to config
    private static final IRI ROOT_ONTOLOGY = IRI.create("http://www.manchester.ac.uk/root.owl");
    private static final String RENDERER_LABEL = "label";

    private OWLOntologyManager mngr;

    private OWLOntology activeOntology;

    private SynchronizedOWLReasoner reasoner;

    private OWLReasonerManager reasonerManager;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider uriShortFormProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator comparator;

    private ServerConfig config;

    private Map<URI, OWLOntologyIRIMapper> baseMapper = Maps.newHashMap();

    private final Set<ActiveOntologyProvider.Listener> listeners = Sets.newHashSet();

    private boolean serverIsDead = false;

    private OWLOntology rootOntology;

    private URL baseUrl;

    protected URLScheme urlScheme;

    public OWLHTMLKitImpl(OWLOntologyManager mngr) {

        this.mngr = mngr;

        this.config = new ServerConfig();

        createRootOntology();

        mngr.addOntologyLoaderListener(ontLoadListener);

        // always default to trying the URI of the ontology
        mngr.addIRIMapper(new NonMappingOntologyIRIMapper());

        setActiveOntology(rootOntology);

        reasonerManager = new OWLReasonerManagerImpl(this);
    }

    @Override
    public URL getBaseUrl(){
        return baseUrl;
    }

    @Override
    public void setBaseUrl(URL baseUrl) {
        this.baseUrl = baseUrl;
    }

    @Override
    public URLScheme getURLScheme() {
        if (urlScheme == null){
            urlScheme = new RestURLScheme(this);
        }
        return urlScheme;
    }

    @Override
    public String getCurrentLabel() {
        String ontHash = getOntConfig().getHash();
        String servHash = getConfig().getHash();
        return ontHash + "_" + servHash;
    }

    @Override
    public boolean isActive() {
        return !serverIsDead;
    }

    @Override
    public OntologyConfig getOntConfig() {
        return OntologyConfig.ontConfigFor(getActiveOntology());
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

    public void loadOntologies(final OntologyConfig ontConfig) {
        OWLOntologyIRIMapper mapper = ontologyIRI -> ontConfig.documentFor(ontologyIRI).orNull();
        mngr.addIRIMapper(mapper);

        for (OntologyMapping mapping : ontConfig.getMappings()){
            if (!mapping.getOntologyIRI().equals(ROOT_ONTOLOGY)){
                try {
                    mngr.loadOntology(mapping.getLocationIRI());
                }
                catch (OWLOntologyDocumentAlreadyExistsException | OWLOntologyAlreadyExistsException e){
                    // do nothing - as we're not trying to load in order just keep going
                }
                catch (OWLOntologyCreationException e) {
                    logger.warn("Problem loading " + mapping, e);
                }
            }
        }

        mngr.removeIRIMapper(mapper);

        resetRootImports();
    }

    public void clearOntologies() {

        final Set<OWLOntology> onts = mngr.getOntologies();
        onts.remove(rootOntology);

        for (OWLOntology ont : onts){
            mngr.removeOntology(ont);
        }

        resetRootImports();

        setActiveOntology(rootOntology);
    }

    private void loadedOntology(OWLOntology ont) {
        if (ont != null){
            logger.info("loaded " + getOntologyIdString(ont));
        }

        resetRootImports();

        if (!getActiveOntology().equals(rootOntology)){
            setActiveOntology(ont);
        }
        else{
            clear();
        }
    }

    public Optional<OWLOntology> getOntologyForIRI(IRI iri) {
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(ontology.getOntologyID().getVersionIRI().orNull())){
                return Optional.of(ontology);
            }
        }
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(ontology.getOntologyID().getOntologyIRI().orNull())){
                return Optional.of(ontology);
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
        return ont.getOntologyID().getDefaultDocumentIRI().transform(new Function<IRI, String>(){

            @Nullable
            @Override
            public String apply(IRI iri) {
                return iri.toString();
            }
        }).or(ont.getOWLOntologyManager().getOntologyDocumentIRI(ont).toString());
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

    public synchronized OWLReasoner getOWLReasoner() {
        if (!isActive()){
            throw new RuntimeException("Cannot getOWLReasoner - server is dead");
        }

        if (reasoner == null){

            String selectedReasoner = config.getReasoner();

            try {
                logger.debug("Creating reasoner: " + selectedReasoner);

                OWLReasoner r = reasonerManager.getReasoner(selectedReasoner);

                if (r == null || !r.isConsistent()){
                    // set the reasoner back to Structural
                    r = reasonerManager.getReasoner(OWLReasonerManagerImpl.STRUCTURAL);
                    if (r == null){
                        throw new RuntimeException("Cannot create " + OWLReasonerManagerImpl.STRUCTURAL);
                    }
                    logger.warn("Could not create reasoner: " + selectedReasoner + ". Setting the reasoner back to " + OWLReasonerManagerImpl.STRUCTURAL);
                }

                reasoner = new SynchronizedOWLReasoner(r);
            }
            catch (Throwable e) {
                throw new RuntimeException(selectedReasoner + ": " + e.getClass().getSimpleName() + " - " + e.getMessage(), e);
            }
        }
        return reasoner;
    }

    public Comparator<OWLObject> getComparator() {
        if (!isActive()){
            throw new RuntimeException("Cannot getComparator - server is dead");
        }
        if (comparator == null){
            comparator = new OWLObjectComparator(getShortFormProvider());
        }
        return comparator;
    }

    public OWLEntityFinder getFinder() {
        if (!isActive()){
            throw new RuntimeException("Cannot getFinder - server is dead");
        }

        if (finder == null){
            finder = new OWLEntityFinderImpl(getNameCache(), getOWLOntologyManager().getOWLDataFactory(), this);
        }

        return finder;
    }


    public OWLEntityChecker getOWLEntityChecker() {
        if (!isActive()){
            throw new RuntimeException("Cannot getOWLEntityChecker - server is dead");
        }
        if (owlEntityChecker == null){
            owlEntityChecker = new ShortFormEntityChecker(getNameCache());
        }
        return owlEntityChecker;
    }

    public ShortFormProvider getShortFormProvider() {
        if (!isActive()){
            throw new RuntimeException("Cannot getShortFormProvider - server is dead");
        }

        if (shortFormProvider == null){
            String ren = config.getRenderer();
            shortFormProvider = new FixedSimpleShortFormProvider();
            if (ren.equals(RENDERER_LABEL)){
                shortFormProvider = new LabelShortFormProvider(this, shortFormProvider);
            }
        }
        return shortFormProvider;
    }


    public OntologyIRIShortFormProvider getOntologyShortFormProvider() {
        if (uriShortFormProvider == null){
            uriShortFormProvider = new OntologyShortFormProvider(getRootOntology());
        }
        return uriShortFormProvider;
    }

    public void dispose() {

        urlScheme = null;
        baseUrl = null;

        clearOntologies();

        mngr = null;

        serverIsDead = true;
    }

    public OWLOntology getRootOntology() {
        return rootOntology;
    }

    @Override
    public ServerConfig getConfig() {
        return config;
    }

    @Override
    public void setConfig(ServerConfig serverConfig) {
        if (this.config != null) {
            if (!serverConfig.getReasoner().equals(this.config.getReasoner())) {
                cleanReasoner();
            }
            if (!serverConfig.getRenderer().equals(this.config.getRenderer()) ||
                    !serverConfig.getLabelAnnotationIri().equals(this.config.getLabelAnnotationIri()) ||
                    !serverConfig.getLabelPropertyIri().equals(this.config.getLabelPropertyIri()) ||
                    !serverConfig.getLabelLang().equals(this.config.getLabelLang())) {
                clearRendererCache();
            }
        }
        this.config = serverConfig;
    }

    public void clear() {
        clearRendererCache();
        comparator = null;
    }


    private void cleanReasoner() {
        if (reasoner != null){
            reasonerManager.dispose(reasoner.getDelegate());
            reasoner = null;
        }
    }

    private void clearRendererCache() {
        if (shortFormProvider != null){
            shortFormProvider.dispose();
            shortFormProvider = null;
            comparator = null;
        }
        if (finder != null){
            finder.dispose();
            finder = null;
        }
        if (nameCache != null){
            nameCache.dispose();
            nameCache = null;
        }
        if (owlEntityChecker != null){
            owlEntityChecker = null;
        }
    }


    private void createRootOntology() {
        try {
            rootOntology = mngr.createOntology(ROOT_ONTOLOGY);
            // TODO: add an explanation annotation for the users
            // TODO: and a label "all ontologies"
//            mngr.applyChange(root, new AddOntologyAnnotation(root, mngr.getOWLDataFactory().getOWLA))
            if (mngr.getOntologies().size() > 1){
                resetRootImports();
            }
        }
        catch (OWLOntologyCreationException e) {
            throw new RuntimeException(e);
        }
    }

    private void resetRootImports() {
        Set<OWLOntology> onts = getOntologies();
        onts.remove(rootOntology);

        final Set<OWLOntology> newRoots = getImportRoots(onts);
        final Set<OWLOntology> oldRoots = rootOntology.getImports();
        oldRoots.removeAll(newRoots);
        newRoots.removeAll(rootOntology.getImports());

        final OWLDataFactory df = mngr.getOWLDataFactory();

        final List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

        for (OWLOntology root : newRoots){
            com.google.common.base.Optional<IRI> maybeIRI = getImportIRIForOntology(root);
            if (maybeIRI.isPresent()) {
                changes.add(new AddImport(rootOntology, df.getOWLImportsDeclaration(maybeIRI.get())));
            }
        }

        for (OWLOntology root : oldRoots){
            com.google.common.base.Optional<IRI> maybeIRI = getImportIRIForOntology(root);
            if (maybeIRI.isPresent()) {
                changes.add(new RemoveImport(rootOntology, df.getOWLImportsDeclaration(maybeIRI.get())));
            }
        }

        mngr.applyChanges(changes);
    }

    private Set<OWLOntology> getImportRoots(Set<OWLOntology> onts){
        // TODO: handle cyclic imports
        Set<OWLOntology> roots = Sets.newHashSet(onts);
        for (OWLOntology ont : onts){
            roots.removeAll(ont.getImports());
        }
        return roots;
    }

    private com.google.common.base.Optional<IRI> getImportIRIForOntology(OWLOntology root) {
        if (root.isAnonymous()){
            // TODO need a workaround as this will not work
            // see OWL API bug - https://sourceforge.net/tracker/?func=detail&aid=3110834&group_id=90989&atid=595534
            return com.google.common.base.Optional.fromNullable(mngr.getOntologyDocumentIRI(root));
        }
        return root.getOntologyID().getDefaultDocumentIRI();
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (!isActive()){
            throw new RuntimeException("Cannot getNameCache - server is dead");
        }
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
