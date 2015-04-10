package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObjectProperty;

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
public class OWLObjectPropertySummaryDoclet extends AbstractOWLDocDoclet<OWLObjectProperty> {

    public OWLObjectPropertySummaryDoclet(OWLHTMLKit kit) {
        super(kit);

        addDoclet(new OWLEntityTitleDoclet<OWLObjectProperty>(kit));
        addDoclet(new AnnotationsDoclet<OWLObjectProperty>(kit));
        addDoclet(new PropertyCharacteristicsDoclet<OWLObjectProperty>(kit));
        addDoclet(new ObjectPropertyDomainsDoclet(kit));
        addDoclet(new ObjectPropertyRangesDoclet(kit));
        addDoclet(new InversesDoclet(kit));
        addDoclet(new AssertedSuperObjectPropertiesDoclet(kit));
        addDoclet(new AssertedEquivObjectPropertiesDoclet(kit));
        addDoclet(new DisjointObjectPropertiesDoclet(kit));
        addDoclet(new UsageDoclet<OWLObjectProperty>(kit));
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
