package org.coode.www.service;

import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Convert a request to a regex which can be found by the OWLEntityFinder.
 */
@Service
public class SearchService {

    private String wildcard = "*";

    public List<OWLEntity> findByName(final String input, final OWLHTMLKit kit) {

        if ((input == null) || (input.isEmpty())) {
            return Collections.emptyList();
        }

        String searchStr = (input.endsWith(wildcard)) ? ".*" + input.replace("\\" + wildcard, ".*") :
                        ".*" + input + ".*";

        // Sort first by index of search and then alphabetically
        Comparator<OWLEntity> c = new Comparator<>() {
            ShortFormProvider sfp = kit.getShortFormProvider();
            String str = input.replaceAll("\\" + wildcard, "").toLowerCase();

            @Override
            public int compare(OWLEntity o1, OWLEntity o2) {
                String sf1 = sfp.getShortForm(o1).toLowerCase();
                String sf2 = sfp.getShortForm(o2).toLowerCase();
                int i1 = sf1.indexOf(str);
                int i2 = sf2.indexOf(str);
                if (i1 == i2) {
                    return sf1.compareTo(sf2);
                }
                else if (i1 < i2) {
                    return -1;
                }
                return 1;
            }
        };

        return kit.getFinder().getOWLEntities(searchStr).stream().sorted(c).collect(Collectors.toList());
    }
}
