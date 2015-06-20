/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.url.PermalinkURLScheme;
import org.coode.html.util.HTMLUtils;
import org.coode.html.util.URLUtils;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
@Deprecated
public abstract class AbstractTitleDoclet<O extends OWLObject> extends AbstractOWLDocDoclet<O> {

    public static final String ID = "doclet.summary.title";
    public static final String PERMALINK_LABEL = "permalink";

    public AbstractTitleDoclet(OWLHTMLKit kit) {
        super(kit);
    }


    protected void renderHeader(URL pageURL, PrintWriter out) {

        final OWLHTMLKit kit = getOWLHTMLKit();

        out.print("<h2>");
        out.print(getTitle());
        out.println("</h2>");

        final boolean permalink = kit.getHTMLProperties().isSet(OWLHTMLProperty.optionRenderPermalink);
        if (permalink){
            HTMLUtils.renderLink(PERMALINK_LABEL,
                       new PermalinkURLScheme(kit.getURLScheme()).getURLForAbsolutePage(pageURL),
                       "permalink",
                       pageURL,
                       out);
        }

        String subtitle = getSubtitle();
        if (subtitle != null){
            out.print("<h3>");
            out.print(subtitle);
            try {
                // cannot just do new URL() as this allows just about anything
                URL url = new URI(subtitle).toURL();
                URLUtils.renderURLLinks(url, kit.getURLScheme(), kit.getVisibleOntologies(), out);
            }
            catch (URISyntaxException|MalformedURLException|IllegalArgumentException e) {
                // do nothing - no load URL for this entity
            }
            out.println("</h3>");
        }
    }

    public abstract String getTitle();


    public abstract String getSubtitle();


    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return ID;
    }
}