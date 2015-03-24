package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.url.URLScheme;
import org.coode.html.util.URLUtils;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 5, 2010<br><br>
 */
public class SignoutDoclet extends AbstractOWLDocDoclet {

    public SignoutDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {

        final URLScheme urlScheme = getOWLHTMLKit().getURLScheme();

        out.print("<a id='signout' href='");
        out.print(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForRelativePage(OWLHTMLConstants.SIGNOUT_HTML)));
        out.print("' target='");
        out.print(OWLHTMLConstants.LinkTarget._top);
        out.print("'>");
        out.print("<img src='");
        out.print(URLUtils.createRelativeURL(pageURL, urlScheme.getURLForRelativePage("images/close.png")));
        out.print("' width='16' height='16' title='close' />");
        out.println("</a>");
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return "doclet.signout";
    }
}
