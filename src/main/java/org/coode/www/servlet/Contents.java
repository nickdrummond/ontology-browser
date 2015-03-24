package org.coode.www.servlet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.doclet.TabsDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.index.OWLContentsHTMLPage;
import org.coode.html.page.HTMLPage;
import org.coode.www.doclet.CloudIndexDoclet;
import org.coode.www.doclet.SearchOntologiesDoclet;
import org.coode.www.exception.OntServerException;

import java.net.URL;
import java.util.Map;

/**
 * Author: Nick Drummond<br>
 * nick.drummond@cs.manchester.ac.uk<br>
 * http://www.cs.man.ac.uk/~drummond<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 20, 2007<br><br>
 * <p/>
 * code made available under Mozilla Public License (http://www.mozilla.org/MPL/MPL-1.1.html)<br>
 * copyright 2006, The University of Manchester<br>
 */
public class Contents extends AbstractOntologyServerServlet {

    private static final String SEARCH_BOX_ID = "searchBox";

    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        try {
            OWLContentsHTMLPage ren = new OWLContentsHTMLPage(kit);

            ren.setAutoFocusedComponent(SEARCH_BOX_ID);

            int insertPoint = -1;
            final HTMLDoclet tabs = ren.getDoclet(TabsDoclet.ID);
            if (tabs != null){
                insertPoint = ren.indexOf(tabs);
            }
            ren.addDoclet(new SearchOntologiesDoclet(kit, SEARCH_BOX_ID), insertPoint+1);

            ren.addDoclet(new CloudIndexDoclet(kit)); // after all contents

            return ren;
        }
        catch (Exception e) {
            throw new OntServerException(e);
        }
    }

    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; // TODO: implement
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; // TODO: implement
    }
}
