/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.util.OWLUtils;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;

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
public class SuperclassesDoclet extends AbstractOWLElementsDoclet<OWLClass, OWLClassExpression> {

    public SuperclassesDoclet(OWLHTMLKit kit) {
        super("Superclasses", Format.list, kit);
    }

    protected Collection<OWLClassExpression> getAssertedElements(Set<OWLOntology> onts) {
        return getUserObject().getSuperClasses(onts);
    }

    @Override
    protected String getCSSClass(OWLClassExpression object) {
        return super.getCSSClass(object);    //To change body of overridden methods use File | Settings | File Templates.
    }

    @Override
    protected Collection<OWLClassExpression> getInferredElements(Set<OWLOntology> onts) {
        final OWLReasoner r = getOWLHTMLKit().getOWLServer().getOWLReasoner();

        Set<OWLClassExpression> supers = new HashSet<OWLClassExpression>();
        supers.addAll(r.getSuperClasses(getUserObject(), true).getFlattened());
        for (OWLClass ancestor : r.getSuperClasses(getUserObject(), false).getFlattened()){
            for (OWLClassExpression e : ancestor.getSuperClasses(onts)){
                if (e.isAnonymous()){
                    supers.add(e);
                }
            }
        }

        if (OWLUtils.isStructural(r)){
            for (OWLClass equiv : r.getEquivalentClasses(getUserObject())){
                for (OWLClassExpression e : equiv.getSuperClasses(onts)){
                    supers.add(e);
                }
            }
        }
        return supers;
    }
}
