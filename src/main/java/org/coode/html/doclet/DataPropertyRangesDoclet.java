package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;

import java.util.Collection;
import java.util.Set;

@Deprecated
public class DataPropertyRangesDoclet extends AbstractOWLElementsDoclet<OWLDataProperty, OWLDataRange> {

    public DataPropertyRangesDoclet(OWLHTMLKit kit) {
        super("Ranges", Format.list, kit);
    }

    protected Collection<OWLDataRange> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getRanges(getUserObject(), onts);
    }
}
