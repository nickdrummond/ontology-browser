package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

public class AssertedSuperDataPropertiesDoclet
        extends AbstractOWLElementsDoclet<OWLDataProperty, OWLDataPropertyExpression> {

    public AssertedSuperDataPropertiesDoclet(OWLHTMLKit kit) {
        super("Superproperties", Format.list, kit);
    }

    protected Collection<OWLDataPropertyExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getSuperProperties(getUserObject(), onts);
    }
}
