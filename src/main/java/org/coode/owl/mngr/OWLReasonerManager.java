package org.coode.owl.mngr;

import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URL;
import java.util.List;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Mar 29, 2011<br><br>
 */
public interface OWLReasonerManager {

    List<String> getAvailableReasonerNames();

    OWLReasoner getReasoner(String name);

    void setRemote(URL url);

    void dispose(OWLReasoner r);

    void dispose();
}
