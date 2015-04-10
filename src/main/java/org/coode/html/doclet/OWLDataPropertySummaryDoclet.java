package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLDataProperty;

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
@Deprecated
public class OWLDataPropertySummaryDoclet extends AbstractOWLDocDoclet<OWLDataProperty> {

    public OWLDataPropertySummaryDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OWLEntityTitleDoclet<OWLDataProperty>(kit));
        addDoclet(new AnnotationsDoclet<OWLDataProperty>(kit));
        addDoclet(new PropertyCharacteristicsDoclet<OWLDataProperty>(kit));
        addDoclet(new DataPropertyDomainsDoclet(kit));
        addDoclet(new DataPropertyRangesDoclet(kit));
        addDoclet(new AssertedSuperDataPropertiesDoclet(kit));
        addDoclet(new AssertedEquivDataPropertiesDoclet(kit));
        addDoclet(new DisjointDataPropertiesDoclet(kit));
        addDoclet(new UsageDoclet<OWLDataProperty>(kit));
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
        return "doclet.summary.dataproperty";
    }
}
