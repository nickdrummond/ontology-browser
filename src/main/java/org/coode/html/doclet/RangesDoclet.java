/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLPropertyExpression;
import org.semanticweb.owlapi.model.OWLPropertyRange;

import java.util.Collection;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class RangesDoclet<O extends OWLPropertyExpression> extends AbstractOWLElementsDoclet<O, OWLPropertyRange> {

    public RangesDoclet(OWLHTMLKit kit) {
        super("Ranges", Format.list, kit);
    }

    protected Collection<OWLPropertyRange> getAssertedElements(Set<OWLOntology> onts) {
        return getUserObject().getRanges(onts);
    }
}
