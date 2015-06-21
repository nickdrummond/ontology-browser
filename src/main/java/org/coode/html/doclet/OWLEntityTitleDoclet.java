/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import com.google.common.base.Optional;
import org.coode.owl.mngr.NamedObjectType;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.kit.impl.OWLHTMLProperty;
import org.coode.www.service.GeoService;
import org.coode.www.service.MediaService;
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
@Deprecated
public class OWLEntityTitleDoclet<O extends OWLEntity> extends AbstractTitleDoclet<O> {

    private final MediaService mediaService;

    public OWLEntityTitleDoclet(OWLHTMLKit kit) {
        super(kit);
        mediaService = new MediaService();
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

        if (mediaService.isImageURL(getUserObject().getIRI())){
            out.print("<div class=\"imageHolder\"><img src=\"");
            out.print(getUserObject().getIRI());
            out.println("\" /></div>");
        }
        else if (mediaService.isSoundURL(getUserObject().getIRI())){
            out.print("<EMBED src=\"");
            out.print(getUserObject().getIRI());
            out.println("\" autostart=\"true\" hidden=\"true\"/>");
        }
    }
}
