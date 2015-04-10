package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

@Deprecated
public class DisjointsDoclet extends AbstractOWLElementsDoclet<OWLClass, OWLClassExpression> {

    public DisjointsDoclet(OWLHTMLKit kit) {
        super("Disjoints", Format.csv, kit);
    }

    protected Collection<OWLClassExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getDisjointClasses(getUserObject(), onts);
    }
}
