/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.HTMLUtils;

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
public class MenuItemDoclet extends AbstractOWLDocDoclet {

    private final String label;
    private final URL link;
    private final OWLHTMLConstants.LinkTarget target;


    public MenuItemDoclet(String label, URL link, OWLHTMLConstants.LinkTarget target, OWLHTMLKit kit) {
        super(kit);
        this.label = label;
        this.link = link;
        this.target = target;
        setPinned(true);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.print(" | ");
        HTMLUtils.renderLink(label, link, target, "", isSingleFrameNavigation(), pageURL, out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return "doclet.menu." + label;
    }
}
