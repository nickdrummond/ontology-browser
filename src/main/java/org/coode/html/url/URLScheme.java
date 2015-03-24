package org.coode.html.url;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 11, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 *
 * Interface describing the set of methods required to create unique URL references for ontology entities.
 * Can be used for static or dynamic sites depending on the implementation
 */
public interface URLScheme extends OWLObjectURLRenderer {

    OWLHTMLKit getOWLHTMLKit();
    
    NamedObjectType getType(URL url);

    URL getBaseURL();

    URL getURLForOntologyIndex(OWLOntology ont, NamedObjectType type);
    
    void setAdditionalLinkArguments(String s);

    void clearAdditionalLinkArguments();

    URL getURLForIndex(NamedObjectType type);

    URL getURLForRelativePage(String pageRelativeToBase);

    URL getURLForAbsolutePage(URL pageURL);
}
