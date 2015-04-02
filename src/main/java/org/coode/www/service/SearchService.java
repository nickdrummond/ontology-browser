package org.coode.www.service;

import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * Convert a request to a regex which can be found by the OWLEntityFinder.
 * TODO Ordering of results
 */
@Service
public class SearchService {

    private String wildcard = "*";

    public List<OWLEntity> findByName(String input, final OWLHTMLKit kit) {

        OWLEntityFinder finder = kit.getOWLServer().getFinder();

        if (input == null){
            input = wildcard;
        }
        else if (!input.endsWith(wildcard)){
            input = input + wildcard;
        }

        Set<OWLEntity> results = finder.getOWLEntities("^" + input.replace(wildcard, ".*"));

        if (results.isEmpty()){
            results.addAll(finder.getOWLEntities(".*" + input.replace(wildcard, ".*")));
        }

        return new ArrayList<OWLEntity>(results);
    }
}
