package org.ontbrowser.www.feature.reasoner;

import com.google.common.base.Stopwatch;
import org.ontbrowser.www.util.OWLUtils;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

public class ReasonerFactoryService {

    private static final Logger log = LoggerFactory.getLogger(ReasonerFactoryService.class);

    private final String defaultToldReasoner;
    private final String defaultInferredReasoner;

    private final Map<String, OWLReasonerFactory> facsByName = new HashMap<>();
    private final Map<String, OWLReasoner> reasonerByName = new HashMap<>();

    public ReasonerFactoryService(
            List<ReasonerMomento> momentos,
            String defaultInferredReasoner,
            String defaultToldReasoner
    ) {
        this.defaultInferredReasoner = defaultInferredReasoner;
        this.defaultToldReasoner = defaultToldReasoner;

        for (ReasonerMomento momento : momentos){
            String label = momento.getLabel();
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(momento.getCls()).getDeclaredConstructor().newInstance();
                facsByName.put(label, fac);
                log.info("Reasoner installed: {}", label);
            }
            catch (Throwable e){
                log.info("skipping: {}", label);
            }
        }
    }

    public void clear() {
        log.info("Clearing reasoner cache");
        var reasoners = reasonerByName.values();
        reasonerByName.clear();
        for (var reasoner: reasoners) {
            log.info("Disposing reasoner {}", reasoner.getReasonerName());
            reasoner.dispose();
        }
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
            r = createReasoner(name, ont, key);
        }
        return r;
    }

    private synchronized OWLReasoner createReasoner(String name, OWLOntology ont, String key) {
        OWLReasonerFactory fac = getFactoryFor(name);
        if (fac != null) {
            String ontName = OWLUtils.ontIRI(ont.getOntologyID());
            log.info("Creating {} reasoner for {}", name, ontName);
            OWLReasoner r = new SynchronizedOWLReasoner(fac.createReasoner(ont, new SimpleConfiguration()));
            var stopwatch = Stopwatch.createStarted();
            r.precomputeInferences(
                    InferenceType.CLASS_HIERARCHY,
                    InferenceType.OBJECT_PROPERTY_HIERARCHY,
                    InferenceType.DATA_PROPERTY_HIERARCHY,
                    InferenceType.CLASS_ASSERTIONS,
                    InferenceType.OBJECT_PROPERTY_ASSERTIONS,
                    InferenceType.DATA_PROPERTY_ASSERTIONS,
                    InferenceType.DIFFERENT_INDIVIDUALS,
                    InferenceType.SAME_INDIVIDUAL
            );
            log.info("{} classified {} in {} ms", name, ontName, stopwatch.elapsed().toMillis());
            if (!r.isConsistent()) {
                throw new RuntimeException("Reasoner " + name + " is not consistent");
            }
            reasonerByName.put(key, r);
            return r;
        }
        else {
            throw new RuntimeException("No reasoner found for " + name);
        }
    }

    private String makeKey(String name, OWLOntology ont) {
        return name + "-" + ont.getOntologyID().toString();
    }

    public OWLReasoner getToldReasoner(OWLOntology ont) {
        return getReasoner(defaultToldReasoner, ont);
    }
}
