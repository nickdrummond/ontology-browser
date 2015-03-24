/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.coode.owl.util.OWLUtils;
import org.semanticweb.owlapi.model.*;
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
public class MembersDoclet extends AbstractOWLElementsDoclet<OWLClass, OWLIndividual> {


    public MembersDoclet(OWLHTMLKit kit) {
        super("Members", Format.csv, kit);
    }

    protected Collection<OWLIndividual> getAssertedElements(Set<OWLOntology> onts) {
        Collection<OWLIndividual> members = new HashSet<OWLIndividual>();
        OWLClass cls = getUserObject();
        for (OWLOntology ont : onts){
            for (OWLAxiom ax : ont.getReferencingAxioms(cls)){
                if (ax instanceof OWLClassAssertionAxiom &&
                    ((OWLClassAssertionAxiom)ax).getClassExpression().equals(cls)){
                    members.add(((OWLClassAssertionAxiom)ax).getIndividual());
                }
            }
        }
        return members;
    }

    @Override
    protected Collection<OWLIndividual> getInferredElements(Set<OWLOntology> ontologies) {
        final OWLReasoner r = getOWLHTMLKit().getOWLServer().getOWLReasoner();

        Set<OWLIndividual> members = new HashSet<OWLIndividual>();
        members.addAll(r.getInstances(getUserObject(), true).getFlattened());

        if (OWLUtils.isStructural(r)){
            for (OWLClass equiv : r.getEquivalentClasses(getUserObject())){
                members.addAll(r.getInstances(equiv, true).getFlattened());
            }
        }
        return members;
    }
}
