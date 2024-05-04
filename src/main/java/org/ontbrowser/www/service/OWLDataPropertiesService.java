package org.ontbrowser.www.service;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.characteristics.DataPropertyCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLDataPropertiesService {

    public OWLDataProperty getOWLDataPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        OWLDataFactory df = kit.getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();
        if (getIdFor(owlTopDataProperty).equals(propertyId)) {
            return owlTopDataProperty;
        }

        OWLDataProperty owlBottomDataProperty = df.getOWLBottomDataProperty();
        if (getIdFor(owlBottomDataProperty).equals(propertyId)) {
            return owlBottomDataProperty;
        }

        for (OWLOntology ont : kit.getActiveOntologies()){
            for (OWLDataProperty owlDataProperty: ont.getDataPropertiesInSignature()) {
                if (getIdFor(owlDataProperty).equals(propertyId)){
                    return owlDataProperty;
                }
            }
        }
        throw new NotFoundException("OWLDataProperty", propertyId);
    }

    public String getIdFor(final OWLDataProperty owlDataProperty) {
        return String.valueOf(owlDataProperty.getIRI().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLDataProperty property,
            final OWLOntology ont,
            final Comparator<OWLObject> comparator,
            final List<With> with,
            final int pageSize) {
        return new DataPropertyCharacteristicsBuilder(property, ont, comparator, with, pageSize).getCharacteristics();
    }
}
