package org.ontbrowser.www.renderer;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.StringWriter;

public class MOSStringRenderer {

    private OWLEntityFinder finder;

    private OWLOntology ont;

    public MOSStringRenderer(OWLEntityFinder finder, OWLOntology ont) {
        this.finder = finder;
        this.ont = ont;
    }

    public String render(OWLObject owlObject) {
        StringWriter writer = new StringWriter();
        owlObject.accept(new MOSRenderer(writer, finder, ont));
        return writer.toString();
    }
}
