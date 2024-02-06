package org.coode.www.service;

import org.coode.www.reasoner.SynchronizedOWLReasoner;
import org.coode.www.model.ReasonerMomento;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.reasoner.OWLReasonerRuntimeException;
import org.semanticweb.owlapi.reasoner.SimpleConfiguration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class ReasonerFactoryService {

    private static final Logger logger = LoggerFactory.getLogger(ReasonerFactoryService.class);

    private final String defaultToldReasoner;
    private final String defaultInferredReasoner;

    private final Map<String, OWLReasonerFactory> facsByName = new HashMap<>();
    private final Map<String, OWLReasoner> reasonerByName = new HashMap<>();

    public ReasonerFactoryService(List<ReasonerMomento> momentos, String defaultInferredReasoner, String defaultToldReasoner) {
        this.defaultInferredReasoner = defaultInferredReasoner;
        this.defaultToldReasoner = defaultToldReasoner;

        for (ReasonerMomento momento : momentos){
            String label = momento.getLabel();
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(momento.getCls()).getDeclaredConstructor().newInstance();
                facsByName.put(label, fac);
                logger.info("Reasoner found: " + label);
            }
            catch (Throwable e){
                logger.info("Reasoner cannot be found: " + label);
            }
        }
    }

    public void clear() {
        Collection<OWLReasoner> reasoners = reasonerByName.values();
        reasonerByName.clear();
        for (OWLReasoner reasoner: reasoners) {
            reasoner.dispose();
        }
    }

    public List<String> getAvailableReasoners(){
        return new ArrayList<>(facsByName.keySet());
    }

    public OWLReasonerFactory getFactoryFor(String name) {
        if (name == null) {
            name = defaultInferredReasoner;
        }
        return facsByName.get(name);
    }

    public synchronized OWLReasoner getReasoner(OWLOntology ont) throws OWLReasonerRuntimeException {
        return getReasoner(defaultInferredReasoner, ont);
    }
        /**
         * Get a reasoner.
         * @param name one of the names provided by {@code getAvailableReasonerNames()}.
         * @return an instance of OWLReasoner or null if no match can be found.
         */
    public synchronized OWLReasoner getReasoner(String name, OWLOntology ont) throws OWLReasonerRuntimeException {
        String key = makeKey(name, ont);
        OWLReasoner r = reasonerByName.get(key);
        if (r == null) {
            OWLReasonerFactory fac = getFactoryFor(name);
            if (fac != null) {
                logger.warn("Creating reasoner..." + name + " for ontology " + ont.getOntologyID());
                r = new SynchronizedOWLReasoner(fac.createNonBufferingReasoner(ont, new SimpleConfiguration()));
                reasonerByName.put(key, r);
            }
            else {
                throw new RuntimeException("No reasoner found for " + name);
            }
        }
        return r;
    }

    private String makeKey(String name, OWLOntology ont) {
        return name + "-" + ont.getOntologyID().toString();
    }

    public OWLReasoner getToldReasoner(OWLOntology ont) {
        return getReasoner(defaultToldReasoner, ont);
    }
}
