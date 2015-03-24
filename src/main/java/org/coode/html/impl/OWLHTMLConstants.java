package org.coode.html.impl;

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
public class OWLHTMLConstants extends ServerConstants {

    public static final URL HOME_PAGE = createURL("http://code.google.com/p/ontology-browser/");

    public static String ONTOLOGY_SERVER_NAME = "Ontology Browser";

    public static final String DOCLET_CONFIG = "doclets.config";

    public static final String DEFAULT_ENCODING = "UTF-8";

    public static final String JS_ROOT = "js/";
    public static final String CSS_BASE= "css/";
    public static final String IMAGES_BASE = "images/";

    // Javascript
    public static final String JS_DEFAULT = JS_ROOT + "default.js";
    public static final String JS_DL_QUERY = JS_ROOT + "dlquery.js";
    public static final String JS_TREE = JS_ROOT + "tree.js";
    public static final String AUTO_SUGGEST_JS = JS_ROOT + "bsn.AutoSuggest_2.1_multiword.js";
    public static final String JQUERY_JS = JS_ROOT + "jquery-1.4.3.min.js";
    public static final String GAPHU_JS = JS_ROOT + "gaphu-0.0.9.js";

    // Styles
    public static final String CSS_DEFAULT = CSS_BASE + "default.css";
    public static final String AUTO_SUGGEST_CSS = CSS_BASE + "autosuggest_inquisitor.css";
    public static final String GAPHU_CSS = CSS_BASE + "gaphu-0.0.9.css";

    // Images
    public static final String EXTERNAL_IMAGE = OWLHTMLConstants.IMAGES_BASE + "external.png";
    public static final String LOAD_IMAGE = OWLHTMLConstants.IMAGES_BASE + "download.png";
    public static final String MINIMISE_IMAGE = IMAGES_BASE + "min.png";

    public static final String DEFAULT_EXTENSION = ".html";

    public static final String INDEX_HTML = "index" + DEFAULT_EXTENSION;

    public static final String CONTENTS_HTML = "contents" + DEFAULT_EXTENSION;
    public static final String DL_QUERY_HTML = "dlquery/";
    public static final String QUERY_HTML = "query/";
    public static final String OPTIONS_HTML = "options/";
    public static final String HEADER_HTML = "header" + DEFAULT_EXTENSION;
    public static final String SIGNOUT_HTML = "signout" + DEFAULT_EXTENSION;

    public static final String DL_QUERY_LABEL = "DL Query";
    public static final String LOAD_LABEL = "Load";
    public static final String CONTENTS_LABEL = "Contents";
    public static final String RESTART_LABEL = "Restart";
    public static final String BOOKMARKS_LABEL = "Bookmarks";
    public static final String PERMALINK_LABEL = "permalink";

    public static final String EQUIV_CHAR = "&equiv;";
    public static final String SUBCLASS_CHAR = "&sube;";

    public static final String START_QUERY = "?";
    public static final String EQUALS = "=";
    public static final String PARAM_SEP = "&";
    public static final String SLASH = "/";

    // how far towards the root do we trim the trees
    public static final int DEFAULT_TREE_ANCESTORS_COUNT = 10;

     // actually needs one more layer than displayed, as this is required for determining if leaf
    public static final int DEFAULT_TREE_DESCENDANTS_COUNT = 2;

    // Clouds
    public static final int DEFAULT_CLOUD_THRESHOLD = 8;
    public static final int DEFAULT_CLOUD_ZOOM = 10;

    public static final String INFERRED_CSS_CLASS = "inferred";
    public static final String ASSERTED_CSS_CLASS = "asserted";


    public enum LinkTarget{_top, content, nav, subnav, header, _blank}

    // window modes
    public static final String NO_FRAMES = "no_frames";
    public static final String SHOW_FRAMES = "frames";


    // keys for options on the kit

    // non-external values
    public static final String DEFAULT_INDEX_ALL_URL = "index-all" + DEFAULT_EXTENSION;


    private static URL createURL(String s) {
        try {
            return new URL(s);
        }
        catch (MalformedURLException e) {
            throw new RuntimeException(e);
        }
    }
}
