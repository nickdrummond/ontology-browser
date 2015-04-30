package org.coode.www.renderer;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.io.StringWriter;

public class OWLHTMLRenderer implements ElementRenderer<OWLObject>{

    private final OWLHTMLVisitor rendererVisitor;

    public OWLHTMLRenderer(OWLHTMLKit kit) {
        rendererVisitor = new OWLHTMLVisitor(
                kit.getOWLServer().getShortFormProvider(),
                kit.getOWLServer().getOntologyShortFormProvider(),
                kit.getURLScheme(),
                kit.getVisibleOntologies(),
                kit.getOWLServer().getActiveOntology());
    }

    public String render(OWLObject obj){
        StringWriter writer = new StringWriter();
        rendererVisitor.setWriter(new PrintWriter(writer));
        obj.accept(rendererVisitor);
        writer.flush();
        return writer.getBuffer().toString();
    }
}
