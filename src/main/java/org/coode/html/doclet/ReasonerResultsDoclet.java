/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.www.model.QueryType;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 6, 2008<br><br>
 */
@Deprecated
public class ReasonerResultsDoclet<O extends OWLClassExpression> extends AbstractOWLElementsDoclet<O, OWLEntity> {

    Set<OWLEntity> results;

    public ReasonerResultsDoclet (QueryType type, Set<OWLEntity> results, OWLHTMLKit kit) throws OntServerException {
        super(type.toString(), Format.list, kit);
        this.results = results;
    }

    protected Collection<OWLEntity> getAssertedElements(Set<OWLOntology> onts) {
        return results;
    }
}
