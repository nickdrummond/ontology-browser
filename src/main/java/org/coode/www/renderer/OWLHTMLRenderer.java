package org.coode.www.renderer;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.io.StringWriter;

public class OWLHTMLRenderer implements ElementRenderer<OWLObject>{

    private final OWLHTMLVisitor rendererVisitor;

    public OWLHTMLRenderer(final OWLHTMLKit kit) {
        rendererVisitor = new OWLHTMLVisitor(
                kit.getShortFormProvider(),
                kit.getOntologyShortFormProvider(),
                kit.getURLScheme(),
                kit.getActiveOntologies(),
                kit.getActiveOntology());
    }

    public OWLHTMLRenderer(final OWLHTMLKit kit, final OWLObject activeObject) {
        this(kit);
        rendererVisitor.setActiveObject(activeObject);
    }

    public String render(final OWLObject obj){
        StringWriter writer = new StringWriter();
        rendererVisitor.setWriter(new PrintWriter(writer));
        obj.accept(rendererVisitor);
        writer.flush();
        return writer.getBuffer().toString();
    }

    public String renderFullIRI(final IRI iri) {
        return iri.getIRIString()
                .replaceAll("/(?=[^/])", "/<wbr>")
                .replace("#", "#<wbr>");
    }
}
