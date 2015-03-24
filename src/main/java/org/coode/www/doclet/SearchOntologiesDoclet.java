/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.NamedObjectType;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 4, 2008<br><br>
 */
public class SearchOntologiesDoclet extends AbstractHTMLDoclet {

    public static final String ID = "doclet.search";

    private static final String TITLE_FIND = "Find";
    private static final String DL_QUERY_AUTOCOMPLETE = "dlQuery";

    private AutocompleteDoclet searchboxDoclet;
    private AutocompleteDoclet dlQueryDoclet;

    public SearchOntologiesDoclet(OWLHTMLKit kit, String searchBoxId) {
        searchboxDoclet = new AutocompleteDoclet(kit, searchBoxId, true);
        searchboxDoclet.setSubmitURL(kit.getURLScheme().getURLForIndex(NamedObjectType.entities));
        searchboxDoclet.setParamName("input");
        searchboxDoclet.setTarget(OWLHTMLConstants.LinkTarget.content);
        addDoclet(searchboxDoclet);

        dlQueryDoclet = new AutocompleteDoclet(kit, DL_QUERY_AUTOCOMPLETE, false);
        dlQueryDoclet.setSubmitName("dl query");
        dlQueryDoclet.setSubmitURL(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML));
        dlQueryDoclet.setTarget(OWLHTMLConstants.LinkTarget.subnav);
        dlQueryDoclet.setMultiword(true);
        addDoclet(dlQueryDoclet);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(TITLE_FIND, out, pageURL);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(TITLE_FIND, out);
    }

    public String getID() {
        return ID;
    }
}
