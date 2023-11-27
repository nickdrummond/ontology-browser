package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.CharacteristicsFactory;
import org.coode.www.model.characteristics.DatatypeCharacteristicsBuilder;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

import static java.util.Arrays.asList;

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

    public List<Characteristic> getCharacteristics(OWLDatatype owlDatatype, OWLHTMLKit kit) {
        return new DatatypeCharacteristicsBuilder(owlDatatype, kit.getActiveOntologies(), kit.getComparator()).getCharacteristics();
    }
}
