package org.coode.www.kit.impl;

import org.coode.owl.mngr.ServerConstants;

import java.net.MalformedURLException;
import java.net.URL;
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
@Deprecated
public class OWLHTMLConstants extends ServerConstants {

    public static final String DEFAULT_ENCODING = "UTF-8";

    public static final String PERMALINK_LABEL = "permalink";

    public static final String EQUIV_CHAR = "&equiv;";
    public static final String SUBCLASS_CHAR = "&sube;";

    public static final String START_QUERY = "?";
    public static final String EQUALS = "=";
    public static final String PARAM_SEP = "&";
    public static final String SLASH = "/";

    public static final String INFERRED_CSS_CLASS = "inferred";
    public static final String ASSERTED_CSS_CLASS = "asserted";

    public enum LinkTarget{_top, content, nav, subnav, header, _blank}
}
