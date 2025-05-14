package org.ontbrowser.www.renderer;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.io.OWLObjectRenderer;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.io.StringWriter;

public class MOSStringRenderer implements OWLObjectRenderer {

    private final OWLEntityFinder finder;

    private final OWLOntology ont;

    public MOSStringRenderer(OWLEntityFinder finder, OWLOntology ont) {
        this.finder = finder;
        this.ont = ont;
    }

    @Override
    public void setShortFormProvider(ShortFormProvider shortFormProvider) {
        // do nothing
    }

    public String render(OWLObject owlObject) {
        StringWriter writer = new StringWriter();
        owlObject.accept(new MOSRenderer(writer, finder, ont));
        return writer.toString();
    }
}
