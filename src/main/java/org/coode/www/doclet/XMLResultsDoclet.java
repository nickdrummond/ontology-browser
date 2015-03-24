package org.coode.www.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.html.doclet.Doclet;
import org.coode.html.impl.OWLHTMLConstants;
import org.coode.owl.mngr.OWLServer;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.PrintWriter;
import java.net.URL;
import java.util.Set;

/**
 * Author: drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Aug 20, 2010<br><br>
 */
public class XMLResultsDoclet implements Doclet {

    private OWLServer server;

    private Set<? extends OWLObject> results;

    public XMLResultsDoclet(Set<? extends OWLObject> results, OWLHTMLKit kit) {
        this.server = kit.getOWLServer();
        this.results = results;
    }

    public void renderAll(URL pageURL, PrintWriter out) {
        out.println("<?xml version=\"1.0\" encoding=\"" + OWLHTMLConstants.DEFAULT_ENCODING + "\" ?>");
        out.println("<results>");
        for (OWLObject result : results){
            if (result instanceof OWLEntity){
                OWLEntity entity = (OWLEntity)result;
                final String name = server.getShortFormProvider().getShortForm(entity);
                out.println("<rs id=\"" + entity.getIRI() + "\" info=\"\">" + name + "</rs>");
            }
            else if (result instanceof OWLOntology){
                OWLOntology ont = (OWLOntology)result;
                final String name = server.getOntologyShortFormProvider().getShortForm(ont);
                out.println("<rs id=\"" + ont.getOntologyID().getOntologyIRI() + "\" info=\"\">" + name + "</rs>");
            }
        }
        out.println("</results>");
    }
}
