package org.coode.www.service;

import org.slf4j.LoggerFactory; import org.slf4j.Logger;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
public class ReasonerFactoryService {

    private static final Logger logger = LoggerFactory.getLogger(ReasonerFactoryService.class);

    public static String STRUCTURAL;

    public static String OWLLINK;

    private final String OWLLINK_CONFIG = "org.semanticweb.owlapi.owllink.OWLlinkReasonerConfiguration";

    private Map<String, OWLReasonerFactory> facsByName = new HashMap<String, OWLReasonerFactory>();

    public ReasonerFactoryService(List<String> reasonerFactoryNames) {
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
                logger.info("Reasoner cannot be found: " + reasonerFactoryName);
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
