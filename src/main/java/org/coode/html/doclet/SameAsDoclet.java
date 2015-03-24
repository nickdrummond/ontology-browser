/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.html.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
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
 * Date: Feb 5, 2008<br><br>
 */
public class SameAsDoclet extends AbstractOWLElementsDoclet<OWLNamedIndividual, OWLIndividual> {

    public SameAsDoclet(OWLHTMLKit kit) {
        super("Same As", Format.list, kit);
    }

    protected Collection<OWLIndividual> getAssertedElements(Set<OWLOntology> onts) {
        Set<OWLIndividual> sameAs = new HashSet<OWLIndividual>();
        for (OWLOntology ont : onts){
            sameAs.addAll(getUserObject().getSameIndividuals(ont));
        }
        return sameAs;
    }

    @Override
    protected Collection<OWLIndividual> getInferredElements(Set<OWLOntology> ontologies) {
        Set<OWLIndividual> synonyms = new HashSet<OWLIndividual>();

        final OWLReasoner r = getOWLHTMLKit().getOWLServer().getOWLReasoner();
        final Set<OWLNamedIndividual> namedSynonyms = r.getSameIndividuals(getUserObject()).getEntities();

        synonyms.addAll(namedSynonyms);

        // now get all anon individuals
        for (OWLIndividual syn : namedSynonyms){
            for (OWLOntology ont : ontologies){
                for (OWLIndividual ind : syn.getSameIndividuals(ont)){
                    synonyms.add(ind);
                }
            }
        }

        synonyms.remove(getUserObject());

        return synonyms;
    }
}
