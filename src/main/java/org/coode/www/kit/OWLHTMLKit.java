package org.coode.www.kit;

import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerOptionsAdapter;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.List;
import java.util.Set;

public interface OWLHTMLKit {

    String getID();

    OWLServer getOWLServer();

    @Deprecated
    ServerOptionsAdapter<OWLHTMLProperty> getHTMLProperties();

    @Deprecated
    URL getBaseURL();
    
    URLScheme getURLScheme();

    void setURLScheme(URLScheme urlScheme);

    /**
     * The ontologies that are visible in the browser (reasoner should always use getActiveOntologies)
     * @return ontologies that are to be rendered in the interface (not including the 'system' meta ontology)
     */
    Set<OWLOntology> getVisibleOntologies();

    /**
     * @param label
     */
    void setCurrentLabel(String label);

    String getCurrentLabel();

    void dispose();

    /**
     * Is the kit currently in use?
     * @return true if there are ontologies to browse
     */
    boolean isActive();

    void addUserError(String errorMessage);
    void addUserError(String errorMessage, Throwable e);
    List<String> getUserErrors();
    void clearUserErrors();
}
