package org.coode.www.renderer;

import org.coode.html.url.URLScheme;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLObject;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.Set;

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

    public OWLHTMLRenderer withActiveObject(final OWLObject activeObject) {
        rendererVisitor.setActiveObjects(Collections.singleton(activeObject));
        return this;
    }

    public OWLHTMLRenderer withActiveObjects(final Set<OWLObject> activeObjects) {
        rendererVisitor.setActiveObjects(activeObjects);
        return this;
    }

    public OWLHTMLRenderer withURLScheme(URLScheme urlScheme) {
        rendererVisitor.setURLScheme(urlScheme);
        return this;
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
