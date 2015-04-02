package org.coode.www.service;

import org.coode.owl.mngr.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.Set;

@Service
public class OWLIndividualsService {

    // TODO need to index the entities by ID
    public OWLIndividual getOWLIndividualFor(final String propertyId, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLIndividual owlIndividual: ont.getIndividualsInSignature()) {
                if (getIdFor(owlIndividual).equals(propertyId)) {
                    return owlIndividual;
                }
            }
        }
        throw new NotFoundException("OWLIndividual", propertyId);
    }

    public String getIdFor(final OWLIndividual owlIndividual) {
        if (owlIndividual.isNamed()){
            return String.valueOf(owlIndividual.asOWLNamedIndividual().getIRI().hashCode());
        }
        else {
            return String.valueOf(owlIndividual.asOWLAnonymousIndividual().getID().hashCode());
        }
    }

    public Set<OWLIndividual> getAllIndividuals(final OWLHTMLKit kit) {
        Set<OWLIndividual> individuals = new HashSet<OWLIndividual>();
        for (OWLOntology ont : kit.getVisibleOntologies()) {
            individuals.addAll(ont.getIndividualsInSignature());
        }
        return individuals;
    }

    public HierarchyProvider<OWLNamedIndividual> getHierarchyProvider(OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLNamedIndividual.class);
    }
}
