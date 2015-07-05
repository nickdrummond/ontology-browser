package org.coode.html.doclet;

import org.coode.www.cloud.CloudModel;
import org.coode.www.cloud.OWLCloudModel;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.service.CloudHelper;
import org.semanticweb.owlapi.model.OWLEntity;

import java.awt.*;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;


/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 15, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
@Deprecated
public class CloudDoclet<O extends OWLEntity> extends AbstractHTMLDoclet<O> {

    private static final String ID = "doclet.cloud";

    private static final String SELECTION_COLOR = "#0000FF";

    private OWLHTMLKit kit;

    private O currentSelection;

    private CloudHelper<O> cloudHelper;

    public CloudDoclet(OWLHTMLKit kit, CloudHelper<O> cloudHelper){
        this.kit = kit;
        this.cloudHelper = cloudHelper;
    }

    public void setCurrentSelection(O currentSelection) {
        this.currentSelection = currentSelection;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {

        renderBoxStart(null, out, pageURL);

        out.println("<div class='cloud'>");

        for (O entity : cloudHelper.getModel().getEntities()){
            renderLabel(entity, pageURL, out);
            out.print(" ");
        }

        out.println("</div><!-- cloud -->");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        renderBoxEnd(cloudHelper.getModel().getTitle(), out);
    }

    private void renderLabel(O entity, URL pageURL, PrintWriter out) {
        int score = cloudHelper.getModel().getValue(entity);

        String colour = SELECTION_COLOR;
        if (!entity.equals(currentSelection)){
            final String rgb = Integer.toHexString(cloudHelper.getColor(score).getRGB());
            colour = "#" + rgb.substring(2, rgb.length());
        }
        int size = cloudHelper.getFontSize(score);

        LinkDoclet link = new LinkDoclet<O>(entity, kit);
        link.setCSS("color: " + colour + "; font-size: " + size + "pt;");
        link.addAttribute("title", Integer.toString(score));

        link.renderAll(pageURL, out);
    }

    public String getID() {
        return ID;
    }
}
