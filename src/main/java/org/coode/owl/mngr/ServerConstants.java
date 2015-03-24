package org.coode.owl.mngr;

import org.semanticweb.owlapi.model.IRI;

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
 * Date: Aug 10, 2007<br><br>
 */
public class ServerConstants {

    public static final boolean COOKIE_SESSION_RECOVERY = true;


    public static final String FOAF_NAME = "http://xmlns.com/foaf/0.1/name";

    public static final IRI LATITUDE = IRI.create("http://www.w3.org/2003/01/geo/wgs84_pos#lat");
    public static final IRI LONGITUDE = IRI.create("http://www.w3.org/2003/01/geo/wgs84_pos#long");
    public static final IRI POINT = IRI.create("http://www.georss.org/georss/point");

    public static final IRI ROOT_ONTOLOGY = IRI.create("http://www.manchester.ac.uk/root.owl");

    public static final String ROOT_ONTOLOGY_RENDERING = "All ontologies";


    public static enum Syntax {man, simple, qd}

    // supported renderers
    public static final String RENDERER_FRAG = "frag";
    public static final String RENDERER_LABEL = "label";

}
