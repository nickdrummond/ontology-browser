/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Feb 5, 2008<br><br>
 */
public class OntologyAnnotationsDoclet extends AbstractOWLElementsDoclet<OWLOntology, OWLAnnotation> {

    public OntologyAnnotationsDoclet(OWLHTMLKit kit) {
        super("Annotations", ElementsDoclet.Format.list, kit);
    }

    protected Collection<OWLAnnotation> getAssertedElements(Set<OWLOntology> onts) {
        return getUserObject().getAnnotations();
    }
}
