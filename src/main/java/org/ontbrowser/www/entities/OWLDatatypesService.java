package org.ontbrowser.www.entities;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.entities.characteristics.Characteristic;
import org.ontbrowser.www.entities.characteristics.DatatypeCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLDatatypesService {

    public OWLDatatype getOWLDatatypeFor(String propertyId, OWLOntology ont) throws NotFoundException {
        OWLDataFactory df = ont.getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();
        if (getIdFor(owlTopDatatype).equals(propertyId)) {
            return owlTopDatatype;
        }
        
        for (OWLDatatype owlDatatype: ont.getDatatypesInSignature(Imports.INCLUDED)) {
            if (getIdFor(owlDatatype).equals(propertyId)){
                return owlDatatype;
            }
        }
        throw new NotFoundException("OWLDatatype", propertyId);
    }

    public String getIdFor(final OWLDatatype owlDatatype) {
        return String.valueOf(owlDatatype.getIRI().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLDatatype owlDatatype,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new DatatypeCharacteristicsBuilder(owlDatatype, ont, comparator, with, pageSize).getCharacteristics();
    }
}
