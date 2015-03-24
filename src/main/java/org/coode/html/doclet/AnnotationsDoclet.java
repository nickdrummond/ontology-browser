/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * Author: Nick Drummond<br>
 * http://www.cs.man.ac.uk/~drummond/<br><br>
 * <p/>
 * The University Of Manchester<br>
 * Bio Health Informatics Group<br>
 * Date: Jan 25, 2008<br><br>
 */
public class AnnotationsDoclet<O extends OWLEntity> extends AbstractOWLElementsDoclet<O, OWLAnnotation> {

    public AnnotationsDoclet(OWLHTMLKit kit) {
        super("Annotations", ElementsDoclet.Format.list, kit);
    }

    protected Collection<OWLAnnotation> getAssertedElements(Set<OWLOntology> onts) {
        Set<OWLAnnotation> annots = new HashSet<OWLAnnotation>();
        for (OWLOntology ont : onts){
            annots.addAll(getUserObject().getAnnotations(ont));
        }
        return annots;
    }
}
