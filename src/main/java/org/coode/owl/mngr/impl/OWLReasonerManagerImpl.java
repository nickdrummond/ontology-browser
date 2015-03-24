package org.coode.owl.mngr.impl;

import org.coode.owl.mngr.ActiveOntologyProvider;
import org.coode.owl.mngr.OWLReasonerManager;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyLoaderListener;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.reasoner.*;

import java.lang.reflect.Constructor;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Mar 24, 2011<br><br>
 */
public class OWLReasonerManagerImpl implements OWLReasonerManager {

    public static String STRUCTURAL;
    public static String OWLLINK;

    private final String OWLLINK_CONFIG = "org.semanticweb.owlapi.owllink.OWLlinkReasonerConfiguration";

    private static Map<String, OWLReasonerFactory> facsByName = new HashMap<String, OWLReasonerFactory>();

    // Lazy binding
    private static final String[] reasonerFactoryNames = {
            "org.semanticweb.owlapi.reasoner.structural.StructuralReasonerFactory",
            "uk.ac.manchester.cs.jfact.JFactFactory",
            "org.semanticweb.HermiT.Reasoner$ReasonerFactory",
            OWLLINK = "org.semanticweb.owlapi.owllink.OWLlinkHTTPXMLReasonerFactory",
            "uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory",
            "com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory"
    };

    static{
        for (String reasonerFactoryName : reasonerFactoryNames){
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(reasonerFactoryName).newInstance();
                facsByName.put(fac.getReasonerName(), fac);

                // assuming the structural reasoner is first
                if (STRUCTURAL == null){
                    STRUCTURAL = fac.getReasonerName();
                }
            }
            catch (Throwable e){
                System.out.println("Reasoner cannot be found: " + reasonerFactoryName);
            }
        }

        for (String name : facsByName.keySet()){
            System.out.println("Reasoner found: " + name);
        }
    }


    private Map<String, OWLReasoner> reasoners = new HashMap<String, OWLReasoner>();

    private OWLOntologyManager mngr;

    private ActiveOntologyProvider activeOntProvider;

    private URL remote = null;


    public OWLReasonerManagerImpl(ActiveOntologyProvider activeOntProvider) {

        this.activeOntProvider = activeOntProvider;

        mngr = activeOntProvider.getActiveOntology().getOWLOntologyManager();

        mngr.addOntologyLoaderListener(new OWLOntologyLoaderListener(){

            public void startedLoadingOntology(LoadingStartedEvent loadingStartedEvent) {
            }

            public void finishedLoadingOntology(LoadingFinishedEvent loadingFinishedEvent) {
                dispose();
            }
        });

        activeOntProvider.addActiveOntologyListener(new ActiveOntologyProvider.Listener(){

            public void activeOntologyChanged(OWLOntology ont) {
                dispose();
            }
        });
    }

    public List<String> getAvailableReasonerNames(){
        return new ArrayList<String>(facsByName.keySet());
    }

    /**
     * Get a reasoner.
     * @param name one of the names provided by {@code getAvailableReasonerNames()}. This defaults to STRUCTURAL if null.
     * @return an instance of OWLReasoner or null if no match can be found.
     */
    public OWLReasoner getReasoner(String name) throws OWLReasonerRuntimeException {
        if (name == null){
            name = STRUCTURAL;
        }

        OWLReasoner reasoner = reasoners.get(name);
        if (reasoner == null){
            final OWLReasonerFactory fac = facsByName.get(name);
            if (fac != null){
                OWLReasonerConfiguration config = getConfiguration(fac);
                if (config != null){
                    reasoner = fac.createNonBufferingReasoner(activeOntProvider.getActiveOntology(), config);
                }
            }
        }
        return reasoner;
    }

    public void setRemote(URL url) {
        this.remote = url;
    }

    // TODO progress monitor?
    private OWLReasonerConfiguration getConfiguration(OWLReasonerFactory fac) {

        if (fac.getClass().getName().equals(OWLLINK)){

            URL remote = getRemote();
            if (remote == null){
                return null;
            }

            try {
                // Create the OWLLink configuration by reflection
                Class<OWLReasonerConfiguration> owlLinkConfig = (Class<OWLReasonerConfiguration>) Class.forName(OWLLINK_CONFIG);
                Constructor<OWLReasonerConfiguration> constructor = owlLinkConfig.getConstructor(URL.class);
                return constructor.newInstance(remote);
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        return new SimpleConfiguration();
    }

    private URL getRemote() {
        return remote;
    }

    public void dispose(OWLReasoner r){
        for (String name : reasoners.keySet()){
            if (reasoners.get(name).equals(r)){
                r.dispose();
                reasoners.remove(name);
                return;
            }
        }
    }

    public void dispose() {
        for (OWLReasoner r : reasoners.values()){
            r.dispose();
        }
        reasoners.clear();
    }
}

