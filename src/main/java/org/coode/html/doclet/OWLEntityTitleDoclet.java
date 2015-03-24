/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLProperty;
import org.coode.html.util.URLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.semanticweb.owlapi.model.OWLEntity;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public class OWLEntityTitleDoclet<O extends OWLEntity> extends AbstractTitleDoclet<O> {


    public OWLEntityTitleDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public String getTitle() {
        final O object = getUserObject();
        String title = "";
        if (!isShowMiniHierarchiesEnabled()){
            title = NamedObjectType.getType(object).getSingularRendering() + ": ";
        }
        return title + getOWLHTMLKit().getOWLServer().getShortFormProvider().getShortForm(object);
    }

    public String getSubtitle() {
        return getUserObject().getIRI().toString();
    }

    private boolean isShowMiniHierarchiesEnabled() {
        return getOWLHTMLKit().getHTMLProperties().isSet(OWLHTMLProperty.optionShowMiniHierarchies);
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        super.renderHeader(pageURL, out);

        if (URLUtils.isImageURL(getUserObject().getIRI())){
            out.print("<div class=\"imageHolder\"><img src=\"");
            out.print(getUserObject().getIRI());
            out.println("\" /></div>");
        }
        else if (URLUtils.isSoundURL(getUserObject().getIRI())){
            out.print("<EMBED src=\"");
            out.print(getUserObject().getIRI());
            out.println("\" autostart=\"true\" hidden=\"true\"/>");
        }

        URLUtils.Loc loc = URLUtils.getLocation(getUserObject(), getOWLHTMLKit().getVisibleOntologies());
        if (loc != null){
            out.println("<div class=\"googlemaps\">");
            out.print("<iframe width=\"425\" height=\"350\" frameborder=\"0\" scrolling=\"no\" src=\"http://maps.google.com/maps?q=");
            out.print(loc.latitude);
            out.print(",");
            out.print(loc.longitude);
            out.print("&amp;ie=UTF8&amp;z=14&amp;ll=");
            out.print(loc.latitude);
            out.print(",");
            out.print(loc.longitude);
            out.print("&amp;output=embed\"></iframe>");
            out.println("</div>");
        }
    }
}
