/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.url;

import org.semanticweb.owlapi.model.OWLObject;

import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 12, 2008<br><br>
 */
public interface OWLObjectURLRenderer {

    URL getURLForOWLObject(OWLObject owlObject);

    OWLObject getOWLObjectForURL(URL url);

}
