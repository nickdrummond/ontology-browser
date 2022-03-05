package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLIndividualsService {

    // TODO need to index the entities by ID
    public OWLNamedIndividual getOWLIndividualFor(final String propertyId, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getActiveOntologies()){
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

    public List<Characteristic> getCharacteristics(OWLNamedIndividual owlIndividual, OWLHTMLKit kit) {
        Set<OWLOntology> activeOntologies = kit.getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        ShortFormProvider shortFormProvider = kit.getShortFormProvider();

        List<Characteristic> characteristics = fac.getAnnotationCharacteristics(owlIndividual, activeOntologies, comparator, shortFormProvider);

        fac.getTypes(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getObjectPropertyAssertions(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getNegativeObjectPropertyAssertions(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getDataPropertyAssertions(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getNegativeDataPropertyAssertions(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getUsage(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getSameAs(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);
        fac.getDifferentFrom(owlIndividual, activeOntologies, comparator).ifPresent(characteristics::add);

        return characteristics;
    }

    public OWLNamedIndividual getFirstIndividual(final OWLHTMLKit kit) throws NotFoundException {

        List<OWLNamedIndividual> inds = new ArrayList<>(kit.getActiveOntology().getIndividualsInSignature(Imports.INCLUDED));
        if (inds.isEmpty()) {
            throw new NotFoundException("OWLIndividual", "any");
        }
        else {
            Collections.sort(inds, kit.getComparator());
            return inds.get(0);
        }
    }
}
