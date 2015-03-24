package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jun 4, 2010<br><br>
 */
public class OWLClassSelectDoclet extends AbstractOWLElementsDoclet<OWLClass, OWLClass>{

    public OWLClassSelectDoclet(String name, OWLHTMLKit kit) {
        super(name, Format.list, kit);
    }

    @Override
    protected Collection<OWLClass> getAssertedElements(Set<OWLOntology> ontologies) {
        Set<OWLClass> elements = new HashSet<OWLClass>();
        for (OWLOntology ont : ontologies){
            elements.addAll(ont.getClassesInSignature());
        }
        return elements;
    }

    @Override
    protected void renderHeader(URL pageURL, PrintWriter out) {
        out.println("<div class='owlselector'>");
        super.renderHeader(pageURL, out);
    }

    @Override
    protected void renderFooter(URL pageURL, PrintWriter out) {
        super.renderFooter(pageURL, out);
        out.println("</div> <!-- owlselector -->");
    }
}
