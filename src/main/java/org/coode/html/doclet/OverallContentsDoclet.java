/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.url.URLScheme;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.owl.util.OWLUtils;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 *
 * @@TODO seperate this off into doclets
 */
public class OverallContentsDoclet extends AbstractOWLDocDoclet{

    public static final String ID = "doclet.contents.extended";

    private String title = "Contents";

    public OverallContentsDoclet(OWLHTMLKit kit, String title) {
        super(kit);
        this.title = title;
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {

        OWLHTMLKit kit = getOWLHTMLKit();

        final Set<OWLOntology> visibleOntologies = kit.getVisibleOntologies();
        final URLScheme urlScheme = kit.getURLScheme();


        renderBoxStart(title, out, pageURL);
        out.println("<ul>");
        if (visibleOntologies.size() > 1){
            out.println("<li>");
            HTMLUtils.renderLink("All " + NamedObjectType.ontologies.getPluralRendering(),
                       kit.getURLScheme().getURLForIndex(NamedObjectType.ontologies),
                       OWLHTMLConstants.LinkTarget.subnav, null,
                       isSingleFrameNavigation(), pageURL, out);
        }
        else{
            final OWLOntology activeOnt = kit.getOWLServer().getActiveOntology();
            String ontID = kit.getOWLServer().getOntologyShortFormProvider().getShortForm(activeOnt);
            final URL ontURL = urlScheme.getURLForOWLObject(activeOnt);
            out.println("<li>");
            HTMLUtils.renderLink(ontID, ontURL, OWLHTMLConstants.LinkTarget.content, null, isSingleFrameNavigation(), pageURL, out);
        }
        out.println("</li>");

        for (NamedObjectType type : NamedObjectType.entitySubtypes()){
            Set<OWLEntity> allEntities = new HashSet<OWLEntity>();
            for (OWLOntology ont : visibleOntologies){
                allEntities.addAll(OWLUtils.getOWLEntitiesFromOntology(type, ont));
            }

            renderIndexLink(allEntities.size(), type, pageURL, out);
        }

        out.println("</ul>");
        HTMLUtils.renderBoxEnd(title, out);
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        // do nothing
    }

    private void renderIndexLink(int count, NamedObjectType type, URL pageURL, PrintWriter out) {
        if (count > 0){
            URL indexURL = getOWLHTMLKit().getURLScheme().getURLForIndex(type);
            out.println("<li>");
            String label = type.getPluralRendering();
            HTMLUtils.renderLink(label, indexURL, OWLHTMLConstants.LinkTarget.subnav, null, isSingleFrameNavigation(), pageURL, out);
            out.println(" (" + count + ")");
            out.println("</li>");
        }
    }

    public String getID() {
        return ID;
    }

    public void setTitle(String title) {
        this.title = title;
    }
}