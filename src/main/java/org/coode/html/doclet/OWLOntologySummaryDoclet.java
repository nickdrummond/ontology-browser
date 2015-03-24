package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URL;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 4, 2010<br><br>
 */
public class OWLOntologySummaryDoclet extends AbstractOWLDocDoclet<OWLOntology> {

    public OWLOntologySummaryDoclet(OWLHTMLKit kit) {
        super(kit);
        addDoclet(new OntologyTitleDoclet(kit));
        addDoclet(new OntologyAnnotationsDoclet(kit));

        final OntologyContentsDoclet referencesDoclet = new OntologyContentsDoclet(kit);
        referencesDoclet.setTitle("References");
        addDoclet(referencesDoclet);

        addDoclet(new OntologyImportsDoclet(kit));
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.write("<div class='summary'>");
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        out.write("</div> <!-- summary -->");
    }

    public String getID() {
        return "doclet.summary.objectproperty";
    }
}
