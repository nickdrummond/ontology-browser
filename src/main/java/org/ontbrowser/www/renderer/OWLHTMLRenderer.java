package org.ontbrowser.www.renderer;

import org.ontbrowser.www.url.URLScheme;
import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Collections;
import java.util.Set;

public class OWLHTMLRenderer implements ElementRenderer<OWLObject>{

    private final OWLHTMLVisitor rendererVisitor;
    private final MOSStringRenderer stringRenderer;

    public OWLHTMLRenderer(final ShortFormProvider sfp,
                           final OntologyIRIShortFormProvider ontSfp,
                           final URLScheme urlScheme,
                           final OWLOntology ont,
                           final OWLEntityFinder finder) {

        stringRenderer = new MOSStringRenderer(finder, ont);
        rendererVisitor = new OWLHTMLVisitor(
                sfp,
                ontSfp,
                urlScheme,
                ont,
                finder);
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

    @Override
    public String render(OWLObject object){
        StringWriter writer = new StringWriter();
        rendererVisitor.setWriter(new PrintWriter(writer));
        object.accept(rendererVisitor);
        writer.flush();
        return writer.getBuffer().toString();
    }

    // Used by templates
    public String renderFullIRI(final IRI iri) {
        return iri.getIRIString()
                .replaceAll("/(?=[^/])", "/<wbr>")
                .replace("#", "#<wbr>");
    }

    public String renderAsPlainString(final OWLObject obj) {
        return stringRenderer.render(obj);
    }
}
