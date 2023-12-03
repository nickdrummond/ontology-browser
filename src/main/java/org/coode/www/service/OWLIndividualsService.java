package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.AxiomWithMetadata;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.IndividualCharacteristicsBuilder;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

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

    public List<Characteristic> getInferredCharacteristics(
            OWLNamedIndividual owlIndividual,
            OWLOntology activeOntology,
            OWLOntologyManager mngr, OWLReasoner reasoner) {
        OWLDataFactory df = mngr.getOWLDataFactory();
        try {
            // TODO materialise into an actual ontology
            IRI inferredIRI = IRI.create("inferred");
            OWLOntology ont = (mngr.contains(inferredIRI)) ?
                    mngr.getOntology(inferredIRI) :
                    mngr.createOntology(inferredIRI);
            List<AxiomWithMetadata> inf = activeOntology.objectPropertiesInSignature(Imports.INCLUDED)
                    .flatMap(p -> reasoner.objectPropertyValues(owlIndividual, p)
                            .map(obj -> df.getOWLObjectPropertyAssertionAxiom(p, owlIndividual, obj))
                            .filter(ax -> !activeOntology.containsAxiom(ax, true))
                            .map(ax -> new AxiomWithMetadata("Inferred Relations", ax, ax, ont)))
                    .collect(Collectors.toList());
            return List.of(new Characteristic(owlIndividual, "Inferred Relations", inf));
        } catch (OWLOntologyCreationException e) {
            throw new RuntimeException(e);
        }
    }
}
