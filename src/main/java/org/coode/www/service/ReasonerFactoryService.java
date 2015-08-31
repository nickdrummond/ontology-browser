package org.coode.www.service;

import org.coode.www.model.ReasonerMomento;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class ReasonerFactoryService {

    private static final Logger logger = LoggerFactory.getLogger(ReasonerFactoryService.class);

    // TODO urgh!
    public static String STRUCTURAL;

    // TODO urgh!
    public static String OWLLINK;

    private final String OWLLINK_CONFIG = "org.semanticweb.owlapi.owllink.OWLlinkReasonerConfiguration";

    private Map<String, OWLReasonerFactory> facsByName = new HashMap<String, OWLReasonerFactory>();

    public ReasonerFactoryService(List<ReasonerMomento> reasoners) {
        for (ReasonerMomento reasoner : reasoners){
            try {
                final OWLReasonerFactory fac = (OWLReasonerFactory) Class.forName(reasoner.getCls()).newInstance();
                facsByName.put(fac.getReasonerName(), fac);

                // assuming the structural reasoner is first
                if (STRUCTURAL == null){
                    STRUCTURAL = fac.getReasonerName();
                }
            }
            catch (Throwable e){
                logger.info("Reasoner cannot be found: " + reasoner);
            }
        }

        for (String name : facsByName.keySet()){
            logger.info("Reasoner found: " + name);
        }
    }

    public List<String> getAvailableReasoners(){
        return new ArrayList<String>(facsByName.keySet());
    }
}
