package org.coode.html.url;

import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.kit.OWLHTMLKit;

import java.net.URL;

/**
 * Interface describing the set of methods required to create unique URL references for ontology entities.
 * Can be used for static or dynamic sites depending on the implementation
 */
public interface URLScheme extends OWLObjectURLRenderer {

    OWLHTMLKit getOWLHTMLKit();

    URL getBaseURL();

    URL getURLForIndex(NamedObjectType type);

    URL getURLForRelativePage(String pageRelativeToBase);
}
