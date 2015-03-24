package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;

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
public class OWLAnnotationPropertySummaryDoclet extends AbstractOWLDocDoclet<OWLAnnotationProperty> {

    public OWLAnnotationPropertySummaryDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OWLEntityTitleDoclet<OWLAnnotationProperty>(kit));
        addDoclet(new AnnotationsDoclet<OWLAnnotationProperty>(kit));
        addDoclet(new AnnotationPropertyDomainsDoclet(kit));
        addDoclet(new AnnotationPropertyRangesDoclet(kit));
        addDoclet(new AnnotationPropertySuperPropertiesDoclet(kit));
        addDoclet(new UsageDoclet<OWLAnnotationProperty>(kit));
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
        return "doclet.summary.annotationproperty";
    }
}
