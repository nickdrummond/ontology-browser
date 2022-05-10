package org.coode.www.service;

import org.coode.owl.mngr.impl.SynchronizedOWLReasoner;
import org.coode.www.model.ReasonerMomento;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ReasonerFactoryService {

    private static final Logger logger = LoggerFactory.getLogger(ReasonerFactoryService.class);

    private String DEFAULT_REASONER_LABEL;

    private Map<String, OWLReasonerFactory> facsByName = new HashMap<String, OWLReasonerFactory>();
    private Map<String, OWLReasoner> reasonerByName = new HashMap<>();

    public ReasonerFactoryService(List<ReasonerMomento> momentos, String defaultLabel) {
        DEFAULT_REASONER_LABEL = defaultLabel;

        for (ReasonerMomento momento : momentos){
            String label = momento.getLabel();
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(momento.getCls()).newInstance();
                facsByName.put(label, fac);
                logger.info("Reasoner found: " + label);
            }
            catch (Throwable e){
                logger.info("Reasoner cannot be found: " + label);
            }
        }
    }

    public List<String> getAvailableReasoners(){
        return new ArrayList<>(facsByName.keySet());
    }

    public OWLReasonerFactory getFactoryFor(String name) {
        if (name == null) {
            name = DEFAULT_REASONER_LABEL;
        }
        return facsByName.get(name);
    }

    public synchronized OWLReasoner getReasoner(OWLOntology ont) throws OWLReasonerRuntimeException {
        return getReasoner(null, ont);
    }
        /**
         * Get a reasoner.
         * @param name one of the names provided by {@code getAvailableReasonerNames()}.
         * @return an instance of OWLReasoner or null if no match can be found.
         */
    public synchronized OWLReasoner getReasoner(String name, OWLOntology ont) throws OWLReasonerRuntimeException {
        if (name == null) {
            name = DEFAULT_REASONER_LABEL;
        }
        OWLReasoner r = reasonerByName.get(name);
        if (r == null) {
            OWLReasonerFactory fac = getFactoryFor(name);
            if (fac != null) {
                logger.warn("Creating reasoner..." + name);
                r = new SynchronizedOWLReasoner(fac.createNonBufferingReasoner(ont, new SimpleConfiguration()));
                reasonerByName.put(name, r);
            }
            else {
                throw new RuntimeException("No reasoner found for " + name);
            }
        }
        return r;
    }
}
