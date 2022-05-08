package org.coode.www.renderer;

import java.util.Optional;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.io.StringWriter;

public class OWLHTMLRenderer implements ElementRenderer<OWLObject>{

    private final OWLHTMLVisitor rendererVisitor;

    public OWLHTMLRenderer(OWLHTMLKit kit, Optional<? extends OWLObject> activeObject) {
        rendererVisitor = new OWLHTMLVisitor(
                kit.getShortFormProvider(),
                kit.getOntologyShortFormProvider(),
                kit.getURLScheme(),
                kit.getActiveOntologies(),
                kit.getActiveOntology(),
                activeObject);
    }

    public String render(OWLObject obj){
        StringWriter writer = new StringWriter();
        rendererVisitor.setWriter(new PrintWriter(writer));
        obj.accept(rendererVisitor);
        writer.flush();
        return writer.getBuffer().toString();
    }

    public String renderFullIRI(IRI iri) {
        return iri.getIRIString().replaceAll("/(?=[^/])", "/<wbr>");
    }
}
