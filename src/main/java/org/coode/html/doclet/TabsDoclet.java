/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.mngr.ServerProperty;
import org.coode.owl.util.OWLUtils;

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
public class TabsDoclet extends AbstractOWLDocDoclet {

    public static final String ID = "doclet.Tabs";

    public TabsDoclet(OWLHTMLKit kit) {
        super(kit);
    }

    public void renderHeader(URL pageURL, PrintWriter out) {
        out.println("\n<div id='tabs'>");

        OWLHTMLKit kit = getOWLHTMLKit();
        final boolean singleFrame = isSingleFrameNavigation();

        boolean entitiesExist = false;

        for (NamedObjectType type : NamedObjectType.values()){

            if (type != NamedObjectType.entities){ // skip entities

                if (OWLUtils.entitiesExist(type, getOWLHTMLKit().getOWLServer().getActiveOntologies())){

                    HTMLUtils.renderLink(type.getPluralRendering(),
                                         kit.getURLScheme().getURLForIndex(type),
                                         OWLHTMLConstants.LinkTarget.subnav,
                                         "",
                                         singleFrame,
                                         pageURL,
                                         out);

                    if (type != NamedObjectType.ontologies){
                        entitiesExist = true;
                    }
                }
                else{
                    out.print(type.getPluralRendering());
                }

                out.println();
            }
        }

        if (entitiesExist){
            HTMLUtils.renderLink("Clouds",
                                 kit.getURLScheme().getURLForRelativePage("cloud/"),
                                 OWLHTMLConstants.LinkTarget.subnav,
                                 "",
                                 singleFrame,
                                 pageURL,
                                 out);
            out.println();


            // add the DL Query tab if the reasoner is enabled
            if (kit.getOWLServer().getProperties().isSet(ServerProperty.optionReasonerEnabled)){
                HTMLUtils.renderLink(OWLHTMLConstants.DL_QUERY_LABEL,
                                     kit.getURLScheme().getURLForRelativePage(OWLHTMLConstants.DL_QUERY_HTML),
                                     OWLHTMLConstants.LinkTarget.subnav,
                                     null,
                                     singleFrame,
                                     pageURL,
                                     out);
                out.println();
            }
        }
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</div> <!-- tabs -->\n\n");
    }

    public String getID() {
        return ID;
    }
}
