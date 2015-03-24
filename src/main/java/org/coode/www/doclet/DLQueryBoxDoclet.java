package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractTitleDoclet;
import org.coode.html.doclet.MessageBoxDoclet;
import org.coode.html.doclet.OWLSelectorDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.ServerProperty;
import org.coode.www.OntologyBrowserConstants;

import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 13, 2010<br><br>
 */
public class DLQueryBoxDoclet extends OWLSelectorDoclet{

    private static final String DL_QUERY_AC_ID = "dlQuery";

    private AutocompleteDoclet acDoclet;

    public DLQueryBoxDoclet(OWLHTMLKit kit) {
        super(kit);

        acDoclet = new AutocompleteDoclet(kit, DL_QUERY_AC_ID, false){

            @Override
            protected void renderHeader(URL pageURL, PrintWriter out) {
                // show the active reasoner
                out.print("<p>Reasoner: <a href=\"");
                out.print(URLUtils.createRelativeURL(pageURL, getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.OPTIONS_HTML)));
                out.print("\">");
                out.print(getOWLHTMLKit().getOWLServer().getProperties().get(ServerProperty.optionReasoner));
                out.print("</a></p>");

                super.renderHeader(pageURL, out);
            }

            @Override
            protected void renderFooter(URL pageURL, PrintWriter out) {
                out.print("<script type=\"text/javascript\">\n" +
                          "new ExpressionEditor(\"" + DL_QUERY_AC_ID + "\",\n" +
                          "{parser : \"../parse/?format=xml&session=" + getOWLHTMLKit().getCurrentLabel() + "\",\n" +
                          " autocomplete: \"../autocomplete/?format=xml&session=" + getOWLHTMLKit().getCurrentLabel() + "\"" +
//                          ",\n" +
//                          " errorHandler: function(e){\n" +
//                          "    var submit = $('#" + DL_QUERY_AC_ID + "Form input[type=submit]');\n" +
//                          "   if (e){\n" +
//                          "      submit.attr('disabled', 'disabled');\n" +
//                          "   }\n" +
//                          "   else{\n" +
//                          "      submit.removeAttr('disabled');\n" +
//                          "   }\n" +
//                          "}" +
                          "});\n" +
                          "</script>");
            }

            @Override
            public List<URL> getRequiredJS() {
                List<URL> js = new ArrayList<URL>();
                js.add(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.JQUERY_JS));
                js.add(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.GAPHU_JS));
                return js;
            }

            @Override
            public Set<URL> getRequiredCSS() {
                return Collections.singleton(getOWLHTMLKit().getURLScheme().getURLForRelativePage(OWLHTMLConstants.GAPHU_CSS));
            }
        };
        acDoclet.setIsTextArea(true);
        acDoclet.setSubmitURL(kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML));
        acDoclet.setSubmitName("query");
        acDoclet.setMultiword(true);

        addDoclet(new AbstractTitleDoclet(kit){

            @Override
            public String getTitle() {
                return OntologyBrowserConstants.DL_QUERY_LABEL;
            }

            @Override
            public String getSubtitle() {
                return null;
            }
        });

        MessageBoxDoclet queryBoxDoclet = new MessageBoxDoclet(null, null);
        queryBoxDoclet.addDoclet(acDoclet);

        addDoclet(queryBoxDoclet);
    }

    public String getID() {
        return DL_QUERY_AC_ID;
    }

    public void setInitialValue(String query) {
        acDoclet.setInitialValue(query);
    }
}
