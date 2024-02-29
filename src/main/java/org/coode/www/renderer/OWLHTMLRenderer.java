package org.coode.www.renderer;

import org.coode.www.url.URLScheme;
import org.coode.www.kit.OWLEntityFinder;
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

    public OWLHTMLRenderer(final ShortFormProvider sfp,
                           final OntologyIRIShortFormProvider ontSfp,
                           final URLScheme urlScheme,
                           final OWLOntology activeOntology,
                           final OWLEntityFinder finder) {
        rendererVisitor = new OWLHTMLVisitor(
                sfp,
                ontSfp,
                urlScheme,
                activeOntology.getImportsClosure(),
                activeOntology,
                finder);
    }

    public OWLHTMLRenderer withBreakOnUnderscore(boolean doBreak) {
        rendererVisitor.setBreakOnUnderscore(doBreak);
        return this;
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

    // Used by templates
    public String renderFullIRI(final IRI iri) {
        return iri.getIRIString()
                .replaceAll("/(?=[^/])", "/<wbr>")
                .replace("#", "#<wbr>");
    }
}
