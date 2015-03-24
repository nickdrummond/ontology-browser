package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLDatatype;

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
public class OWLDatatypeSummaryDoclet extends AbstractOWLDocDoclet<OWLDatatype> {

    public OWLDatatypeSummaryDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OWLEntityTitleDoclet<OWLDatatype>(kit));
        addDoclet(new AnnotationsDoclet<OWLDatatype>(kit));
        addDoclet(new DatatypeDefinitionDoclet(kit));
        addDoclet(new UsageDoclet<OWLDatatype>(kit));
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
        return "doclet.summary.datatype";
    }
}
