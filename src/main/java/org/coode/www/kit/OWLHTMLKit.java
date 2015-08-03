package org.coode.www.kit;

import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerOptionsAdapter;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.coode.www.model.OntologyConfig;
import org.coode.www.model.ServerConfig;
import org.semanticweb.owlapi.model.OWLOntology;

import javax.annotation.Nonnull;
import java.net.URL;
import java.util.Set;

public interface OWLHTMLKit {

    OWLServer getOWLServer();

    ServerConfig getConfig();

    void setConfig(@Nonnull ServerConfig serverConfig);

    @Deprecated
    ServerOptionsAdapter<OWLHTMLProperty> getHTMLProperties();

    @Deprecated
    URL getBaseURL();
    
    URLScheme getURLScheme();

    /**
     * The ontologies that are visible in the browser (reasoner should always use getActiveOntologies)
     * @return ontologies that are to be rendered in the interface (not including the 'system' meta ontology)
     */
    Set<OWLOntology> getVisibleOntologies();

    String getCurrentLabel();

    void dispose();

    /**
     * Is the kit currently in use?
     * @return true if there are ontologies to browse
     */
    boolean isActive();

    OntologyConfig getOntConfig();
}
