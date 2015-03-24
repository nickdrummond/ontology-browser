/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class BlurbDoclet extends AbstractHTMLDoclet {

    private static final String ID = "doclet.blurb";

    private static final String DOCUMENTATION_BOX = "Documentation";


    protected void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(null, out, pageURL);
        out.print("<p style='font-weight: bold;/*text-decoration: underline;*/'><a href=\"");
        out.print(OWLHTMLConstants.HOME_PAGE);
        out.println("\" target=\"docs\">Ontology Browser Homepage</a><br />");
        out.print("<a href=\"");
        out.print(OWLHTMLConstants.HOME_PAGE);
        out.println("wiki/GettingStarted\" target=\"docs\">Getting started</a><br />");
        out.print("<a href=\"");
        out.print(OWLHTMLConstants.HOME_PAGE);
        out.println("wiki/ReleaseNotes\" target=\"docs\">Release notes</a><br />");
        out.print("<a href=\"");
        out.print(OWLHTMLConstants.HOME_PAGE);
        out.println("issues/list\" target=\"docs\">Bugs/Feature requests</a></p>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(DOCUMENTATION_BOX, out);
    }

    public String getID() {
        return ID;
    }
}
