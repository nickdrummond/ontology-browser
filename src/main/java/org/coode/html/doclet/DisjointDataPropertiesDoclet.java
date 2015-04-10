package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

@Deprecated
public class DisjointDataPropertiesDoclet
        extends AbstractOWLElementsDoclet<OWLDataProperty, OWLDataPropertyExpression> {

    public DisjointDataPropertiesDoclet(OWLHTMLKit kit) {
        super("Disjoint Properties", Format.csv, kit);
    }

    protected Collection<OWLDataPropertyExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getDisjointProperties(getUserObject(), onts);
    }
}
