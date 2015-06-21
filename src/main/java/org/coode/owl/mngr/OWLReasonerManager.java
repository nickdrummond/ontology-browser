package org.coode.owl.mngr;

import org.semanticweb.owlapi.reasoner.OWLReasoner;

import java.net.URL;
import java.util.List;

public interface OWLReasonerManager {

    List<String> getAvailableReasonerNames();

    OWLReasoner getReasoner(String name);

    void setRemote(URL url);

    void dispose(OWLReasoner r);

    void dispose();
}
