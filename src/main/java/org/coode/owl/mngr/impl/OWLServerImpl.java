package org.coode.owl.mngr.impl;

import com.google.common.base.Function;
import com.google.common.base.Optional;
import org.coode.owl.hierarchy.*;
import org.coode.owl.mngr.*;
import org.coode.www.renderer.LabelShortFormProvider;
import org.coode.www.renderer.OntologyShortFormProvider;
import org.coode.www.renderer.QuotingBiDirectionalShortFormProvider;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.*;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nullable;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;

public class OWLServerImpl implements OWLServer {

    private static final Logger logger = LoggerFactory.getLogger(OWLServerImpl.class);

    // TODO move to config
    private static final IRI ROOT_ONTOLOGY = IRI.create("http://www.manchester.ac.uk/root.owl");
    private static final String FOAF_NAME = "http://xmlns.com/foaf/0.1/name";
    private static final String RENDERER_FRAG = "frag";
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

    private Map<Class<? extends OWLObject>, HierarchyProvider> hps = new HashMap<Class<? extends OWLObject>, HierarchyProvider>();

    private Map<URI, OWLOntologyIRIMapper> baseMapper = new HashMap<URI, OWLOntologyIRIMapper>();

    private ServerOptionsAdapter<ServerProperty> properties;

    private final Set<Listener> listeners = new HashSet<Listener>();

    private boolean serverIsDead = false;

    private OWLOntology rootOntology;

    private PropertyChangeListener propertyChangeListener = new PropertyChangeListener(){

        public void propertyChange(PropertyChangeEvent propertyChangeEvent) {
            try{
                handlePropertyChange(ServerProperty.valueOf(propertyChangeEvent.getPropertyName()),
                        propertyChangeEvent.getNewValue());
            }
            catch(IllegalArgumentException e){
                // do nothing - a user property
            }
        }
    };

    private OWLOntologyLoaderListener ontLoadListener = new OWLOntologyLoaderListener() {
        public void startedLoadingOntology(LoadingStartedEvent loadingStartedEvent) {
            // do nothing
        }

        public void finishedLoadingOntology(LoadingFinishedEvent loadingFinishedEvent) {
            if (loadingFinishedEvent.isSuccessful() && !loadingFinishedEvent.isImported()){
                OWLOntologyID id = loadingFinishedEvent.getOntologyID();
                OWLOntology ont = mngr.getOntology(id);
                if (ont == null){
                    ont = getOntologyForIRI(loadingFinishedEvent.getDocumentIRI());
                }
                loadedOntology(ont);
            }
        }
    };

    public OWLServerImpl(OWLOntologyManager mngr) {

        this.mngr = mngr;

        createRootOntology();

        mngr.addOntologyLoaderListener(ontLoadListener);

        // always default to trying the URI of the ontology
        mngr.addIRIMapper(new NonMappingOntologyIRIMapper());

        setActiveOntology(rootOntology);

        reasonerManager = new OWLReasonerManagerImpl(this);

        getProperties().set(ServerProperty.optionReasoner, OWLReasonerManagerImpl.STRUCTURAL);
        getProperties().setAllowedValues(ServerProperty.optionReasoner, reasonerManager.getAvailableReasonerNames());
    }

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


    public void loadOntologies(final Map<IRI, IRI> ontMap) {
        OWLOntologyIRIMapper mapper = new OWLOntologyIRIMapper(){
            public IRI getDocumentIRI(IRI ontologyIRI) {
                return ontMap.get(ontologyIRI);
            }
        };
        mngr.addIRIMapper(mapper);

        for (Map.Entry<IRI, IRI> entry : ontMap.entrySet()){
            try {
                if (!entry.getKey().equals(ROOT_ONTOLOGY)){
                    mngr.loadOntology(entry.getValue());
                }
            }
            catch (OWLOntologyDocumentAlreadyExistsException e){
                // do nothing - as we're not trying to load in order just keep going 
            }
            catch (OWLOntologyAlreadyExistsException e){
                // do nothing - as we're not trying to load in order just keep going
            }
            catch (OWLOntologyCreationException e) {
                e.printStackTrace();
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

    public OWLOntology getOntologyForIRI(IRI iri) {
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(ontology.getOntologyID().getVersionIRI())){
                return ontology;
            }
        }
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(ontology.getOntologyID().getOntologyIRI())){
                return ontology;
            }
        }

        // look for an ontology with this location
        for (OWLOntology ontology : getOntologies()){
            if (iri.equals(getOWLOntologyManager().getOntologyDocumentIRI(ontology))){
                return ontology;
            }
        }
        return getAnonymousOntology(iri.toString());
    }

    public ServerOptionsAdapter<ServerProperty> getProperties() {
        if (properties == null){

            properties = new ServerOptionsAdapterImpl<ServerProperty>(new ServerPropertiesImpl());

            properties.setBoolean(ServerProperty.optionReasonerEnabled, false);
            properties.setAllowedValues(ServerProperty.optionReasonerEnabled,
                    Arrays.asList(Boolean.TRUE.toString(), Boolean.FALSE.toString()));

            // make sure the deprecated names are updated on a load
            properties.addDeprecatedNames(ServerProperty.generateDeprecatedNamesMap());

            properties.set(ServerProperty.optionRenderer, RENDERER_LABEL);
            properties.setAllowedValues(ServerProperty.optionRenderer,
                    Arrays.asList(RENDERER_FRAG, RENDERER_LABEL)
            );

            properties.set(ServerProperty.optionLabelUri, OWLRDFVocabulary.RDFS_LABEL.getIRI().toString());
            properties.set(ServerProperty.optionLabelLang, "");

            properties.set(ServerProperty.optionLabelPropertyUri, FOAF_NAME);

            properties.set(ServerProperty.optionActiveOnt, ROOT_ONTOLOGY.toString());
            properties.setAllowedValues(ServerProperty.optionActiveOnt,
                    Collections.singletonList(ROOT_ONTOLOGY.toString()));

            properties.addPropertyChangeListener(propertyChangeListener);
        }
        return properties;
    }

    public OWLOntology getActiveOntology() {
        if (activeOntology == null){
            String ont = getProperties().get(ServerProperty.optionActiveOnt);
            if (ont != null){
                IRI activeOntIRI = IRI.create(ont);
                if (activeOntIRI != null){
                    activeOntology = getOntologyForIRI(activeOntIRI);
                }
            }
        }
        if (activeOntology == null){
            activeOntology = rootOntology;
        }
        return activeOntology;
    }

    public void addActiveOntologyListener(Listener l) {
        listeners.add(l);
    }

    public void removeActiveOntologyListener(Listener l) {
        listeners.add(l);
    }

    private OWLOntology getAnonymousOntology(String id) {
        for (OWLOntology ontology : getOntologies()){
            if (id.equals(ontology.getOntologyID().toString())){
                return ontology;
            }
        }
        return null;
    }

    public void setActiveOntology(OWLOntology ont) {
        if (ont == null){
            ont = activeOntology;
        }

        final OWLOntology activeOnt = getActiveOntology();
        if (!activeOnt.equals(ont)){
            getProperties().set(ServerProperty.optionActiveOnt, getOntologyIdString(ont));
        }
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
        if (isDead()){
            throw new RuntimeException("Cannot getOWLReasoner - server is dead");
        }

        if (reasoner == null){

            try {
                reasonerManager.setRemote(getProperties().getURL(ServerProperty.optionRemote));
            }
            catch (MalformedURLException e) {
                reasonerManager.setRemote(null);
            }

            String selectedReasoner = getProperties().get(ServerProperty.optionReasoner);

            try {
                logger.debug("Creating reasoner: " + selectedReasoner);

                OWLReasoner r = reasonerManager.getReasoner(selectedReasoner);

                if (r == null || !r.isConsistent()){
                    // set the reasoner back to Structural
                    selectedReasoner = OWLReasonerManagerImpl.STRUCTURAL;
                    getProperties().set(ServerProperty.optionReasoner, selectedReasoner);
                    r = reasonerManager.getReasoner(selectedReasoner);
                    if (r == null){
                        throw new RuntimeException("Cannot create " + OWLReasonerManagerImpl.STRUCTURAL);
                    }
//                    throw new RuntimeException("Could not create reasoner: " + selectedReasoner + ". Setting the reasoner back to " + OWLReasonerManagerImpl.STRUCTURAL);
                }

                reasoner = new SynchronizedOWLReasoner(r);
            }
            catch (Throwable e) {
                throw new RuntimeException(selectedReasoner + ": " + e.getClass().getSimpleName() + " - " + e.getMessage(), e);
            }
        }
        return reasoner;
    }

    @SuppressWarnings("unchecked")
    public <N extends OWLObject> HierarchyProvider<N> getHierarchyProvider(Class<N> cls) {
        HierarchyProvider<N> hp = (HierarchyProvider<N>)hps.get(cls);
        if (hp == null){
            if (OWLClass.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new ClassHierarchyProvider(this);
            }
            else if (OWLObjectProperty.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLObjectPropertyHierarchyProvider(this);
            }
            else if (OWLDataProperty.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLDataPropertyHierarchyProvider(this);
            }
            else if (OWLAnnotationProperty.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLAnnotationPropertyHierarchyProvider(this);
            }
            else if (OWLNamedIndividual.class.isAssignableFrom(cls)){
                // TODO the corresponding type is wrong - getParents produces OWLClass, not OWLNamedIndividual
                hp = (HierarchyProvider<N>)new OWLIndividualByClassHierarchyProvider(this);
            }
            else if (OWLDatatype.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OWLDatatypeHierarchyProvider(this);
            }
            else if (OWLOntology.class.isAssignableFrom(cls)){
                hp = (HierarchyProvider<N>)new OntologyHierarchyProvider(this);
            }
        }
        return hp;
    }

    public Comparator<OWLObject> getComparator() {
        if (isDead()){
            throw new RuntimeException("Cannot getComparator - server is dead");
        }
        if (comparator == null){
            comparator = new OWLObjectComparator(getShortFormProvider());
        }
        return comparator;
    }

    public OWLEntityFinder getFinder() {
        if (isDead()){
            throw new RuntimeException("Cannot getFinder - server is dead");
        }

        if (finder == null){
            finder = new OWLEntityFinderImpl(getNameCache(), this);
        }
        return finder;
    }


    public OWLEntityChecker getOWLEntityChecker() {
        if (isDead()){
            throw new RuntimeException("Cannot getOWLEntityChecker - server is dead");
        }
        if (owlEntityChecker == null){
            owlEntityChecker = new ShortFormEntityChecker(getNameCache());
        }
        return owlEntityChecker;
    }

    public ShortFormProvider getShortFormProvider() {
        if (isDead()){
            throw new RuntimeException("Cannot getShortFormProvider - server is dead");
        }

        if (shortFormProvider == null){
            String ren = getProperties().get(ServerProperty.optionRenderer);
            shortFormProvider = new SimpleShortFormProvider();
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

        clearOntologies();

        mngr = null;

        if (properties != null){
            properties.removePropertyChangeListener(propertyChangeListener);
            properties = null;
        }

        serverIsDead = true;
    }

    public boolean isDead() {
        return serverIsDead;
    }

    public OWLOntology getRootOntology() {
        return rootOntology;
    }

    public void clear() {
        resetRendererCache();
        resetHierarchies();
        resetAllowedActiveOntology();
        resetAllowedLabels();
        comparator = null;
    }


    private void resetReasoner() {
        if (reasoner != null){
            reasonerManager.dispose(reasoner.getDelegate());
            reasoner = null;
        }
    }

    private void resetHierarchies() {
        hps.clear();
    }

    private void resetRendererCache() {
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
            Optional<IRI> maybeIRI = getImportIRIForOntology(root);
            if (maybeIRI.isPresent()) {
                changes.add(new AddImport(rootOntology, df.getOWLImportsDeclaration(maybeIRI.get())));
            }
        }

        for (OWLOntology root : oldRoots){
            Optional<IRI> maybeIRI = getImportIRIForOntology(root);
            if (maybeIRI.isPresent()) {
                changes.add(new RemoveImport(rootOntology, df.getOWLImportsDeclaration(maybeIRI.get())));
            }
        }

        mngr.applyChanges(changes);
    }

    private Set<OWLOntology> getImportRoots(Set<OWLOntology> onts){
        // TODO: handle cyclic imports
        Set<OWLOntology> roots = new HashSet<OWLOntology>(onts);
        for (OWLOntology ont : onts){
            roots.removeAll(ont.getImports());
        }
        return roots;
    }

    private Optional<IRI> getImportIRIForOntology(OWLOntology root) {
        if (root.isAnonymous()){
            // TODO need a workaround as this will not work
            // see OWL API bug - https://sourceforge.net/tracker/?func=detail&aid=3110834&group_id=90989&atid=595534
            return Optional.fromNullable(mngr.getOntologyDocumentIRI(root));
        }
        return root.getOntologyID().getDefaultDocumentIRI();
    }

    private void resetAllowedLabels() {
        Set<String> uriStrings = new HashSet<String>();
        for (OWLOntology ont : getActiveOntologies()){
            for (OWLAnnotationProperty p : ont.getAnnotationPropertiesInSignature()){
                uriStrings.add(p.getIRI().toString());
            }
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelUri, new ArrayList<String>(uriStrings));

        Set<String> dataPropStrings = new HashSet<String>();
        for (OWLOntology ont : getActiveOntologies()){
            for (OWLDataProperty p : ont.getDataPropertiesInSignature()){
                dataPropStrings.add(p.getIRI().toString());
            }
        }
        getProperties().setAllowedValues(ServerProperty.optionLabelPropertyUri, new ArrayList<String>(dataPropStrings));
    }

    private void resetAllowedActiveOntology() {
        List<String> ontologies = new ArrayList<String>();
        for (OWLOntology ontology : getOntologies()){
            ontologies.add(getOntologyIdString(ontology));
        }
        getProperties().setAllowedValues(ServerProperty.optionActiveOnt, ontologies);
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (isDead()){
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

        if (baseURI != null){
            if (baseMapper.get(baseURI) == null){
                final BaseURIMapper mapper = new BaseURIMapper(baseURI);
                baseMapper.put(baseURI, mapper);
                mngr.addIRIMapper(mapper);
            }
        }
    }


    private void handlePropertyChange(ServerProperty p, Object newValue) {

        switch(p){
            case optionReasoner:
                resetReasoner();
                break;
            case optionRenderer:     // DROPTHROUGH
            case optionLabelLang:
                resetRendererCache();
                break;
            case optionLabelUri:     // DROPTHROUGH
            case optionLabelPropertyUri:
                try {
                    new URI((String)newValue);
                    resetRendererCache();
                }
                catch (URISyntaxException e) {
                    // invalid URI - do not change the renderer
                }
                break;
            case optionActiveOnt:    // DROPTHROUGH
            case optionShowOntologies:
                activeOntology = null; // this will force it to be taken from the properties

                clear();

                OWLOntology ont = getActiveOntology();

                for (Listener l : listeners){
                    l.activeOntologyChanged(ont);
                }
                break;
        }
    }
}
