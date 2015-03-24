package org.coode.owl.mngr.impl;

import org.apache.log4j.Logger;
import org.coode.owl.mngr.*;
import org.coode.owl.util.MySimpleShortFormProvider;
import org.coode.owl.util.OWLObjectComparator;
import org.coode.owl.util.OWLUtils;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.expression.ShortFormEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.*;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.*;


/**
 * Author: drummond<br>
 * The University Of Manchester<br>
 * Medical Informatics Group<br>
 * Date: Jul 4, 2006<br><br>
 * <p/>
 * nick.drummond@cs.manchester.ac.uk<br>
 * www.cs.man.ac.uk/~drummond<br><br>
 */
public class OWLServerImpl implements OWLServer {

    private static final Logger logger = Logger.getLogger(OWLServerImpl.class.getName());

    private OWLOntologyManager mngr;

    private OWLOntology activeOntology;

    private SynchronizedOWLReasoner reasoner;

    private OWLReasonerManager reasonerManager;

    private ShortFormProvider shortFormProvider;

    private OntologyIRIShortFormProvider uriShortFormProvider;

    private OWLEntityChecker owlEntityChecker;

    private CachingBidirectionalShortFormProvider nameCache;

    private OWLEntityFinder finder;

    private OWLObjectComparator<OWLObject> comparator;

    private Map<String, OWLClassExpressionParser> parsers = new HashMap<String, OWLClassExpressionParser>();

    private Map<Class<? extends OWLObject>, HierarchyProvider> hps = new HashMap<Class<? extends OWLObject>, HierarchyProvider>();

    private Map<URI, OWLOntologyIRIMapper> baseMapper = new HashMap<URI, OWLOntologyIRIMapper>();

    private ServerPropertiesAdapter<ServerProperty> properties;

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

        for (IRI iri : ontMap.keySet()){
            try {
                if (!iri.equals(ServerConstants.ROOT_ONTOLOGY)){
                    mngr.loadOntology(iri);
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


    public OWLOntology reloadOntology(OWLOntology ontology) throws OWLOntologyCreationException {
        URI physicalLocation = getOWLOntologyManager().getOntologyDocumentIRI(ontology).toURI();

        mngr.removeOntology(ontology);

        OWLOntology ont = mngr.loadOntologyFromOntologyDocument(IRI.create(physicalLocation));

        resetRootImports();

        clear();

        return ont;
    }

    /**
     * Required because there are currently no listeners on the manager to tell this has happened
     */
    public void removeOntology(OWLOntology ont) {
        if (ont.equals(rootOntology)){
            logger.warn("Cannot remove the root ontology");
            return;
        }

        final OWLOntology activeOnt = getActiveOntology();

        mngr.removeOntology(ont);

        resetRootImports();

        if (activeOnt.equals(ont)){
            setActiveOntology(rootOntology);
        }

        clear();
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
            logger.info("loaded " + OWLUtils.getOntologyIdString(ont));
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

    public ServerPropertiesAdapter<ServerProperty> getProperties() {
        if (properties == null){

            properties = new ServerPropertiesAdapterImpl<ServerProperty>(new ServerPropertiesImpl());

            properties.setBoolean(ServerProperty.optionReasonerEnabled, false);
            properties.setAllowedValues(ServerProperty.optionReasonerEnabled, Arrays.asList(Boolean.TRUE.toString(),
                                                                                            Boolean.FALSE.toString()));

            // make sure the deprecated names are updated on a load
            properties.addDeprecatedNames(ServerProperty.generateDeprecatedNamesMap());

            properties.set(ServerProperty.optionRenderer, ServerConstants.RENDERER_LABEL);
            properties.setAllowedValues(ServerProperty.optionRenderer, Arrays.asList(ServerConstants.RENDERER_FRAG,
                                                                                     ServerConstants.RENDERER_LABEL));

            properties.set(ServerProperty.optionLabelUri, OWLRDFVocabulary.RDFS_LABEL.getIRI().toString());
            properties.set(ServerProperty.optionLabelLang, "");

            properties.set(ServerProperty.optionLabelPropertyUri, ServerConstants.FOAF_NAME);

            properties.set(ServerProperty.optionActiveOnt, ServerConstants.ROOT_ONTOLOGY.toString());
            properties.setAllowedValues(ServerProperty.optionActiveOnt, Collections.singletonList(ServerConstants.ROOT_ONTOLOGY.toString()));

            properties.addPropertyChangeListener(propertyChangeListener);
        }
        return properties;
    }

    public void resetProperties() {
        properties.removePropertyChangeListener(propertyChangeListener);
        properties = null;
        clear();
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
            getProperties().set(ServerProperty.optionActiveOnt, OWLUtils.getOntologyIdString(ont));
        }
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
            comparator = new OWLObjectComparator<OWLObject>(this);
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
            shortFormProvider = new MySimpleShortFormProvider();
            if (ren.equals(ServerConstants.RENDERER_LABEL)){
                shortFormProvider = new LabelShortFormProvider(this, shortFormProvider);
            }
        }
        return shortFormProvider;
    }


    public OntologyIRIShortFormProvider getOntologyShortFormProvider() {
        if (uriShortFormProvider == null){
            uriShortFormProvider = new OntologyIRIShortFormProvider(){
                @Override
                public String getShortForm(OWLOntology ont) {
                    if (ont == rootOntology){
                        return ServerConstants.ROOT_ONTOLOGY_RENDERING;
                    }
                    if (ont.isAnonymous()){
                        return getOWLOntologyManager().getOntologyDocumentIRI(ont).toString();
                    }
                    return super.getShortForm(ont);
                }
            };
        }
        return uriShortFormProvider;
    }


    public final OWLClassExpressionParser getClassExpressionParser(String type){
        if (isDead()){
            throw new RuntimeException("Cannot getClassExpressionParser - server is dead");
        }

        return parsers.get(type);
    }

    public final void registerDescriptionParser(String syntax, OWLClassExpressionParser parser) {
        parsers.put(syntax, parser);
    }

    public Set<String> getSupportedSyntaxes() {
        return parsers.keySet();
    }

    public void dispose() {

        clearOntologies();

        mngr = null;

        parsers.clear();

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
            rootOntology = mngr.createOntology(ServerConstants.ROOT_ONTOLOGY);
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

        final Set<OWLOntology> newRoots = OWLUtils.getImportRoots(onts);
        final Set<OWLOntology> oldRoots = rootOntology.getImports();
        oldRoots.removeAll(newRoots);
        newRoots.removeAll(rootOntology.getImports());

        final OWLDataFactory df = mngr.getOWLDataFactory();

        List<OWLOntologyChange> changes = new ArrayList<OWLOntologyChange>();

        for (OWLOntology root : newRoots){
            changes.add(new AddImport(rootOntology, df.getOWLImportsDeclaration(getImportIRIForOntology(root))));
        }

        for (OWLOntology root : oldRoots){
            changes.add(new RemoveImport(rootOntology, df.getOWLImportsDeclaration(getImportIRIForOntology(root))));
        }

        mngr.applyChanges(changes);
    }

    private IRI getImportIRIForOntology(OWLOntology root) {
        if (root.isAnonymous()){
            // TODO need a workaround as this will not work
            // see OWL API bug - https://sourceforge.net/tracker/?func=detail&aid=3110834&group_id=90989&atid=595534
            return mngr.getOntologyDocumentIRI(root);
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
            ontologies.add(OWLUtils.getOntologyIdString(ontology));
        }
        getProperties().setAllowedValues(ServerProperty.optionActiveOnt, ontologies);
    }

    private CachingBidirectionalShortFormProvider getNameCache(){
        if (isDead()){
            throw new RuntimeException("Cannot getNameCache - server is dead");
        }
        if (nameCache == null){
            nameCache = new CachingBidirectionalShortFormProvider(){
                protected String generateShortForm(OWLEntity owlEntity) {
                    String shortform = getShortFormProvider().getShortForm(owlEntity);
                    if (shortform.indexOf(" ") > -1){ // if this is a multiword name
                        shortform = "\"" + shortform + "\"";
                    }
                    return shortform;
                }
            };
            // TODO: should names also include all standard xsd datatypes - not just the ones referenced?
            nameCache.rebuild(new ReferencedEntitySetProvider(getActiveOntologies()));
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
