package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

@Deprecated
public class AssertedEquivDataPropertiesDoclet
        extends AbstractOWLElementsDoclet<OWLDataProperty, OWLDataPropertyExpression> {

    public AssertedEquivDataPropertiesDoclet(OWLHTMLKit kit) {
        super("Equivalent Properties", Format.list, kit);
    }

    protected Collection<OWLDataPropertyExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getEquivalentProperties(getUserObject(), onts);
    }
}
