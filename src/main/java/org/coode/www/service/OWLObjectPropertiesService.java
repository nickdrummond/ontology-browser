package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.ObjectPropertyCharacteristicsBuilder;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLObjectPropertiesService {

    public OWLObjectProperty getOWLObjectPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        if (getIdFor(owlTopObjectProperty).equals(propertyId)) {
            return owlTopObjectProperty;
        }

        OWLObjectProperty owlBottomObjectProperty = df.getOWLBottomObjectProperty();
        if (getIdFor(owlBottomObjectProperty).equals(propertyId)) {
            return owlBottomObjectProperty;
        }

        for (OWLOntology ont : kit.getActiveOntologies()){
            for (OWLObjectProperty owlObjectProperty: ont.getObjectPropertiesInSignature()) {
                if (getIdFor(owlObjectProperty).equals(propertyId)){
                    return owlObjectProperty;
                }
            }
        }
        throw new NotFoundException("OWLObjectProperty", propertyId);
    }

    public String getIdFor(final OWLObjectProperty owlObjectProperty) {
        return String.valueOf(owlObjectProperty.getIRI().hashCode());
    }

    public List<Characteristic> getCharacteristics(final OWLObjectProperty property, final OWLHTMLKit kit) {
        return new ObjectPropertyCharacteristicsBuilder(property, kit.getActiveOntologies(), kit.getComparator()).getCharacteristics();
    }
}
