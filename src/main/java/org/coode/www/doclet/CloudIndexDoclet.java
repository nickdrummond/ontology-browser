/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.cloud.CloudType;
import org.coode.html.doclet.AbstractOWLDocDoclet;
import org.coode.html.doclet.OWLSelectorDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.util.OWLUtils;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 4, 2008<br><br>
 */
public class CloudIndexDoclet extends OWLSelectorDoclet {

    public static final String ID = "doclet.index.cloud";

    public CloudIndexDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new AbstractOWLDocDoclet(kit){

            @Override
            protected void renderHeader(URL pageURL, PrintWriter out) {
                renderBoxStart("Clouds", out, pageURL);
                for (CloudType cloud : CloudType.values()){
                    if (OWLUtils.entitiesExist(cloud.getType(), getOWLHTMLKit().getOWLServer().getActiveOntologies())){
                        out.print("<li>");
                        HTMLUtils.renderLink(cloud.getRendering(), getOWLHTMLKit().getURLScheme().getURLForRelativePage("cloud/?type=" + cloud), OWLHTMLConstants.LinkTarget.subnav, "", isSingleFrameNavigation(), pageURL, out);
                        out.println("</li>");
                    }
                }
            }

            @Override
            protected void renderFooter(URL pageURL, PrintWriter out) {
                renderBoxEnd("Clouds", out);
            }

            public String getID() {
                return "doclet.cloud.index.box";
            }
        });
    }
}
