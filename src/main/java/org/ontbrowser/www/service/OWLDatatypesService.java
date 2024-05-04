package org.ontbrowser.www.service;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.characteristics.DatatypeCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLDatatypesService {

    public OWLDatatype getOWLDatatypeFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDatatype owlTopDatatype = df.getTopDatatype();
        if (getIdFor(owlTopDatatype).equals(propertyId)) {
            return owlTopDatatype;
        }
        
        for (OWLOntology ont : kit.getActiveOntologies()){
            for (OWLDatatype owlDatatype: ont.getDatatypesInSignature()) {
                if (getIdFor(owlDatatype).equals(propertyId)){
                    return owlDatatype;
                }
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
