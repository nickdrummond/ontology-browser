package org.coode.www;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.doclet.ElementsDoclet;
import org.coode.html.doclet.HTMLDoclet;
import org.coode.html.impl.OWLHTMLParam;
import org.coode.html.page.HTMLPage;
import org.coode.html.page.OWLDocPage;
import org.coode.html.renderer.ElementRenderer;
import org.coode.www.exception.OntServerException;
import org.coode.www.mngr.SessionManager;
import org.coode.www.servlet.AbstractOntologyServerServlet;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collection;
import java.util.Map;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Apr 11, 2011<br><br>
 */
public class Stats extends AbstractOntologyServerServlet {

    @Override
    protected Doclet handleXMLRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return null; // TODO
    }

    @Override
    protected HTMLPage handleHTMLPageRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        OWLDocPage page = new OWLDocPage(kit);
        page.addDoclet(handleHTMLFragmentRequest(params, kit, pageURL));
        return page;
    }

    @Override
    protected HTMLDoclet handleHTMLFragmentRequest(Map<OWLHTMLParam, String> params, OWLHTMLKit kit, URL pageURL) throws OntServerException {
        return new ElementsDoclet<OWLHTMLKit, OWLHTMLKit>("Running Kits", ElementsDoclet.Format.list){

            @Override
            protected Collection<OWLHTMLKit> getElements() {
                return SessionManager.getRunningKits();
            }

            @Override
            protected ElementRenderer<? super OWLHTMLKit> getElementRenderer() {
                return new ElementRenderer<OWLHTMLKit>(){
                    public void render(OWLHTMLKit object, URL pageURL, PrintWriter out) {
                        out.print(object);
                    }
                };
            }
        };
    }
}
