package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.IndividualCharacteristicsBuilder;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLIndividualsService {

    public OWLNamedIndividual getOWLIndividualFor(final String propertyId, final Set<OWLOntology> ontologies) throws NotFoundException {
        for (OWLOntology ont : ontologies){
            for (OWLNamedIndividual owlIndividual: ont.getIndividualsInSignature()) {
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

    public List<Characteristic> getCharacteristics(final OWLNamedIndividual owlIndividual,
                                                   final Set<OWLOntology> activeOntologies,
                                                   final Comparator<OWLObject> comparator) {

        return new IndividualCharacteristicsBuilder(owlIndividual, activeOntologies, comparator).getCharacteristics();
    }

    public OWLNamedIndividual getFirstIndividual(final OWLHTMLKit kit) throws NotFoundException {

        List<OWLNamedIndividual> inds = new ArrayList<>(kit.getActiveOntology().getIndividualsInSignature(Imports.INCLUDED));
        if (inds.isEmpty()) {
            throw new NotFoundException("OWLIndividual", "any");
        }
        else {
            inds.sort(kit.getComparator());
            return inds.get(0);
        }
    }
}
