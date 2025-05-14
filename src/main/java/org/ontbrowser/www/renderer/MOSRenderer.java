package org.ontbrowser.www.renderer;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.manchestersyntax.renderer.ManchesterOWLSyntaxObjectRenderer;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

import java.io.Writer;
import java.util.Set;

/**
 * Adapts the default MOS rendering
 * - annotation axioms render the entity (if there is one) instead of IRI
 */
class MOSRenderer extends ManchesterOWLSyntaxObjectRenderer {
    private final OWLEntityFinder finder;
    private final OWLOntology ont;

    public MOSRenderer(Writer writer,
                       OWLEntityFinder finder,
                       OWLOntology ont) {
        super(writer, finder.getShortFormProvider());
        this.finder = finder;
        this.ont = ont;
    }

    @Override
    public void visit(IRI iri) {
        Set<OWLEntity> matchingEntities = finder.getOWLEntities(iri, ont);
        if (matchingEntities.size() == 1) {
            // Render the entity
            matchingEntities.iterator().next().accept(this);
        }
        else {
            // Either 0 or more than 1 entity matches this IRI
            this.write(iri.toQuotedString());
        }
    }
}
