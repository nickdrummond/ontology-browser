package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

public class DisjointObjectPropertiesDoclet
        extends AbstractOWLElementsDoclet<OWLObjectProperty, OWLObjectPropertyExpression> {

    public DisjointObjectPropertiesDoclet(OWLHTMLKit kit) {
        super("Disjoint Properties", Format.csv, kit);
    }

    protected Collection<OWLObjectPropertyExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getDisjointProperties(getUserObject(), onts);
    }
}
