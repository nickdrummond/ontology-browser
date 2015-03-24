/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.util.HTMLUtils;
import org.coode.owl.mngr.HierarchyProvider;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.net.URL;
import java.util.List;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 7, 2008<br><br>
 */
public class HierarchyDoclet<O extends OWLObject> extends LeafNodeDoclet<O> {

    private String title;

    public HierarchyDoclet(String title, OWLHTMLKit kit, HierarchyProvider<O> model) {
        super(kit, null, model);
        this.title = title;
        setPinned(false);
    }

    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<div class='owlselector'>");
        renderBoxStart(title, out, pageURL);
        out.println("<ul class='minihierarchy " + getHierarchyProvider().getNodeClass().getSimpleName() + "'>");
    }

    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.println("</ul>");
        HTMLUtils.renderBoxEnd(title, out);
        out.println("</div><!-- owlselector -->");
    }

    

    /**
     * This will be the <em>focus</em> object, not the root
     * @param object
     */
    public void setUserObject(O object) {
        clear();
        super.setUserObject(object);
        if (object != null){
            final List<O> roots = asOrderedList(getHierarchyProvider().getRoots());
            
            for (O root : roots){
                NodeDoclet<O> nodeDoclet = new NodeDoclet<O>(getOWLHTMLKit(), root, getHierarchyProvider());
                nodeDoclet.setAutoExpandEnabled(isAutoExpandSubs());
                nodeDoclet.setShowSubs(isShowSubsEnabled());
                nodeDoclet.setUserObject(object);
                addDoclet(nodeDoclet);
            }
        }
    }
}
