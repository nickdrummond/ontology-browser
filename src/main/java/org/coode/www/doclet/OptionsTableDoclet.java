/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.owl.mngr.ServerPropertiesAdapter;
import org.coode.owl.mngr.ServerProperty;

import java.io.PrintWriter;
import java.net.URL;
import java.util.List;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 21, 2008<br><br>
 */
public class OptionsTableDoclet extends AbstractOWLDocDoclet {

    public static final String ID = "doclet.options";

    private Map<OWLHTMLParam, String> params;


    public OptionsTableDoclet(Map<OWLHTMLParam, String> params, OWLHTMLKit kit) {
        super(kit);
        this.params = params;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {

        renderProperties(getOWLHTMLKit().getHTMLProperties(),
                         OWLHTMLProperty.getCurrent(),
                         "Look and Feel",
                         pageURL,
                         out);

        renderProperties(getOWLHTMLKit().getOWLServer().getProperties(),
                         ServerProperty.getCurrent(),
                         "Model",
                         pageURL,
                         out);
    }


    private <E extends Enum> void renderProperties(ServerPropertiesAdapter<E> properties, List<E> keys, String title, URL pageURL, PrintWriter out) {

        renderBoxStart(title, out, pageURL);

        String lastProperty = params.get(OWLHTMLParam.property);

        out.println("<table style='margin-bottom: 10px;'>");
        for (E key : keys){
            String value = properties.get(key);
            String css = "";
            if (key.name().equals(lastProperty)){
                css = " style='font-weight: bolder'";
            }
            out.println("<tr" + css + ">");
            out.println("<td class='key'>" + key.toString() + "</td>");
            out.println("<td>");

            List<String> allowedValues = properties.getAllowedValues(key);
            if (allowedValues.isEmpty()){
                out.println("<form method='POST' action='./'>");
                renderEditor(key, value, out);
                out.println("<input type='submit' value='ok' /></form>");
            }
//            else if (allowedValues.contains(OWLHTMLConstants.TRUE)){
//                renderCheckbox(key, value, out);
//            }
            else{
                // messy - should add Doclet
                OptionSelectorDoclet selectorDoclet = new OptionSelectorDoclet(getOWLHTMLKit(), key.name(), value, allowedValues);
                selectorDoclet.renderAll(pageURL, out);
            }
            out.println("</td></tr>");
        }

        out.println("</table>");

        renderBoxEnd(title, out);
    }


    private void renderCheckbox(Enum option, boolean value, PrintWriter out) {
        out.println("<input type='hidden' name='" + OWLHTMLParam.property + "' value='" + option.name() + "' />");

        out.print("<input type='checkbox' name='");
        out.print(OWLHTMLParam.value);
        out.print("'");
        if (value){
            out.print(" checked");
        }
        out.print(">");
    }


    private void renderEditor(Enum option, String value, PrintWriter out) {
        out.println("<input type='hidden' name='" + OWLHTMLParam.property + "' value='" + option.name() + "' />");
        out.print("<input type='text' name='" + OWLHTMLParam.value + "' value='" + value + "' />");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
    }

    public String getID() {
        return ID;
    }
}
