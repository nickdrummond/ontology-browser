package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.HTMLUtils;
import org.coode.html.util.URLUtils;
import org.coode.www.OntologyBrowserConstants;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jul 26, 2010<br><br>
 */
public class TitleDoclet extends AbstractHTMLDoclet {

    private OWLHTMLKit kit;


    public TitleDoclet(OWLHTMLKit kit) {
        this.kit = kit;
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println();
        HTMLUtils.renderLink("Help", OntologyBrowserConstants.HELP_PAGE, OWLHTMLConstants.LinkTarget._blank, "help", true, pageURL, out);
        out.println();

        out.print("<h1>");
        if (!pageURL.equals(kit.getBaseURL())){
            out.print("<a href=\"");
            out.print(URLUtils.createRelativeURL(pageURL, kit.getBaseURL()));
            out.print("\">");
            out.print(OntologyBrowserConstants.ONTOLOGY_SERVER_NAME);
            out.print("</a>");
        }
        else{
            out.print(OntologyBrowserConstants.ONTOLOGY_SERVER_NAME);
        }
        out.println("</h1>");
        out.print(" v");
        out.print(OntologyBrowserConstants.VERSION);
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    public String getID() {
        return "TITLE";
    }
}
