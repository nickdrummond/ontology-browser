package org.coode.html.index;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.BookmarksDoclet;
import org.coode.html.doclet.OntologyContentsDoclet;
import org.coode.html.doclet.OverallContentsDoclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.html.page.OWLDocPage;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 12, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class OWLContentsHTMLPage extends OWLDocPage<OWLOntology> {

    private OverallContentsDoclet contentsDoclet;

    public OWLContentsHTMLPage(OWLHTMLKit kit) {
        super(kit);

        contentsDoclet = new OverallContentsDoclet(kit, OWLHTMLConstants.CONTENTS_LABEL);

        addDoclet(contentsDoclet);

        setTitle(OWLHTMLConstants.CONTENTS_LABEL);

        final Set<OWLOntology> visibleOntologies = getOWLHTMLKit().getVisibleOntologies();

        if (visibleOntologies.size() > 1){
            for (OWLOntology ont : visibleOntologies){
                OntologyContentsDoclet doclet = new OntologyContentsDoclet(getOWLHTMLKit());
                doclet.setPinned(true);
                doclet.setUserObject(ont);
                addDoclet(doclet);
            }
        }

        addDoclet(new BookmarksDoclet(kit));
    }

    public void setTitle(String title) {
        contentsDoclet.setTitle(title);
        super.setTitle(title);
    }
}
