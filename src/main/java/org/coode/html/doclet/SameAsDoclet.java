/*
* Copyright (C) 2007, University of Manchester
*/
package org.coode.html.doclet;

import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.search.EntitySearcher;

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
@Deprecated
public class SameAsDoclet extends AbstractOWLElementsDoclet<OWLNamedIndividual, OWLIndividual> {

    public SameAsDoclet(OWLHTMLKit kit) {
        super("Same As", Format.list, kit);
    }

    protected Collection<OWLIndividual> getAssertedElements(Set<OWLOntology> onts) {
        return EntitySearcher.getSameIndividuals(getUserObject(), onts);
    }

    @Override
    protected Collection<OWLIndividual> getInferredElements(Set<OWLOntology> ontologies) {
        Set<OWLIndividual> synonyms = new HashSet<OWLIndividual>();

        final OWLReasoner r = getOWLHTMLKit().getOWLServer().getOWLReasoner();
        final Set<OWLNamedIndividual> namedSynonyms = r.getSameIndividuals(getUserObject()).getEntities();

        synonyms.addAll(namedSynonyms);

        // now get all anon individuals
        for (OWLIndividual syn : namedSynonyms){
            synonyms.addAll(EntitySearcher.getSameIndividuals(syn, ontologies));
        }

        synonyms.remove(getUserObject());

        return synonyms;
    }
}
