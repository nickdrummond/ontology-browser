/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.page;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractHTMLDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.OWLDocPage;
import org.coode.www.OntologyBrowserConstants;
import org.coode.www.doclet.DLQueryBoxDoclet;

import java.io.PrintWriter;
import java.net.URL;
import java.util.List;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class DLQueryHTMLPage extends OWLDocPage {

    private DLQueryBoxDoclet acDoclet;

    public DLQueryHTMLPage(OWLHTMLKit kit) {
        super(kit);

        setTitle(OntologyBrowserConstants.DL_QUERY_LABEL);

        acDoclet = new DLQueryBoxDoclet(kit);

        addDoclet(acDoclet);

        setAutoFocusedComponent(acDoclet.getID());

        // TODO get rid of this as the hardcoding makes it difficult for proxies
        addOnLoad("queryURL=\"" + kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.QUERY_HTML).getPath() + "\";");

        addDoclet(new AbstractHTMLDoclet(){

            protected void renderHeader(URL pageURL, PrintWriter out) {
                out.println("<div id='resultsForm'>");
            }

            protected void renderFooter(URL pageURL, PrintWriter out) {
                out.println("</div>");
            }

            public String getID() {
                return "doclet.results";
            }
        });
    }

    public void setQuery(String query){
        
//        String jsAction = "sendQuery();";
//
//        if (query != null && query.length() > 0){
//            addOnLoad(jsAction);
//        }
        acDoclet.setInitialValue(query);
    }

    public List<URL> getRequiredJS() {
        List<URL> js = super.getRequiredJS();
        js.add(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.JS_DL_QUERY));
        return js;
    }
}
