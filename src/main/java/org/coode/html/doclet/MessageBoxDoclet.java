/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.util.HTMLUtils;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 24, 2008<br><br>
 */
public class MessageBoxDoclet extends AbstractHTMLDoclet {

    public static final String ID = "doclet.Message";

    private final String title;
    private String message;

    private String cssClass = "codebox";

    public MessageBoxDoclet(String title, String message) {
        this.title = title;
        this.message = message;
    }

    public void renderHeader(URL pageURL, PrintWriter out) {
        renderBoxStart(out);
        // print message if there is one
        if (message != null && message.length() > 0){
            out.print("<pre>");
            out.print(message);
            out.println("</pre>");
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        HTMLUtils.renderBoxEnd(title, out);
    }

    public String getID() {
        return ID;
    }

    protected final void renderBoxStart(PrintWriter out) {
        if (title != null){
            out.println("<h2>" + title + "</h2>");
        }
        out.print("<div class='");
        out.print(cssClass);
        if (title != null){
            out.print("' id='" + title.toLowerCase().replace(" ", "_"));
        }
        out.println("'>");
    }

    public void setClass(String cssClass) {
        this.cssClass = cssClass;
    }
}
