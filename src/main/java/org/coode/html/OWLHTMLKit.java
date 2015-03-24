package org.coode.html;

import org.coode.html.doclet.HTMLDocletFactory;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.url.URLScheme;
import org.coode.owl.mngr.OWLServer;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.Comparator;
import java.util.List;
import java.util.Set;
/*
* Copyright (C) 2007, University of Manchester
*
* Modifications to the initial code base are copyright of their
* respective authors, or their employers as appropriate.  Authorship
* of the modifications may be determined from the ChangeLog placed at
* the end of this file.
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.

* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.

* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*/

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Oct 2, 2007<br><br>
 */
public interface OWLHTMLKit {

    String getID();

    OWLServer getOWLServer();

    ServerPropertiesAdapter<OWLHTMLProperty> getHTMLProperties();

    void resetProperties();

    URL getBaseURL();
    
    URLScheme getURLScheme();

    void setURLScheme(URLScheme urlScheme);

    HTMLDocletFactory getDocletFactory();

    Comparator<OWLObject> getOWLObjectComparator();

    /**
     * The ontologies that are visible in the browser (reasoner should always use getActiveOntologies)
     * @return ontologies that are to be rendered in the interface (not including the 'system' meta ontology)
     */
    Set<OWLOntology> getVisibleOntologies();

//    void setOntologyVisible(OWLOntology ontology, boolean visible);


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
