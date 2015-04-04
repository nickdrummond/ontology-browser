package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

public class AssertedEquivObjectPropertiesDoclet
        extends AbstractOWLElementsDoclet<OWLObjectProperty, OWLObjectPropertyExpression> {

    public AssertedEquivObjectPropertiesDoclet(OWLHTMLKit kit) {
        super("Equivalent Properties", Format.list, kit);
    }

    protected Collection<OWLObjectPropertyExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getEquivalentProperties(getUserObject(), onts);
    }
}
