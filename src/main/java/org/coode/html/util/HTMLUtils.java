package org.coode.html.util;

import org.coode.html.impl.OWLHTMLConstants;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Nov 15, 2010<br><br>
 */
public class HTMLUtils {


    /**
     * Makes sure the link does not target a frame when single frame navigation is on (to prevent popups in other windows).
     *
     * @param name readable link text
     * @param href page to link to
     * @param target may be null. If set, this is only added if in multiframe mode
     * @param cssClass may be null. The CSS class applied to the anchor element
     * @param singleFrame - if true, the target should be ignored
     * @param pageURL the current page URL, so that links can be made relative
     * @param out printwriter to write to
     */
    public static void renderLink(String name, URL href, OWLHTMLConstants.LinkTarget target, String cssClass, boolean singleFrame, URL pageURL, PrintWriter out) {
        final String relURL = URLUtils.createRelativeURL(pageURL, href);
        if (relURL.length() == 0){
            out.print("<span class='currentpage'>");
            out.print(name);
            out.print("</span>");
        }
        else{
            out.print("<a href='" + relURL + "'");

            if (cssClass != null){
                out.print(" class='" + cssClass + "'");
            }

            // if the linktarget is another window or we are in a frames view add the target
            if (target != null && (target == OWLHTMLConstants.LinkTarget._blank || !singleFrame)){
                out.print(" target='" + target + "'");
            }

            out.print(" >" + name + "</a>");
        }
    }

    public static void renderBoxStart(String name, String id, PrintWriter out, URL pageURL) {
        out.println();
        if (name != null){
            out.print("<div id='");
            out.print(id);
            out.println("'>");

            out.print("<h4>");
            out.print(name);
            out.println("</h4>");
        }
        
        out.print("<div class='codebox");
        if (name == null){
            out.print("' id='");
            out.print(id);
        }
        out.println("'>");
    }

    public static void renderBoxEnd(String name, PrintWriter out) {
        out.println("</div>");
        if (name != null){
            out.print("</div>");
            out.print("<!-- ");
            out.print(name.toLowerCase());
            out.println(" -->");
        }
        out.println();
    }
}
