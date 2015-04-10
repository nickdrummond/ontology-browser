package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

@Deprecated
public class DataPropertyDomainsDoclet extends AbstractOWLElementsDoclet<OWLDataProperty, OWLClassExpression> {

    public DataPropertyDomainsDoclet(OWLHTMLKit kit) {
        super("Domains", Format.list, kit);
    }

    protected Collection<OWLClassExpression> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getDomains(getUserObject(), onts);
    }
}
