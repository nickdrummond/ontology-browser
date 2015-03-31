package org.coode.www.service;

import org.coode.html.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

@Service
public class OWLDataPropertiesService {

    // TODO need to index the entities by ID
    public OWLDataProperty getOWLDataPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLDataProperty owlTopDataProperty = df.getOWLTopDataProperty();
        if (getIdFor(owlTopDataProperty).equals(propertyId)) {
            return owlTopDataProperty;
        }

        OWLDataProperty owlBottomDataProperty = df.getOWLBottomDataProperty();
        if (getIdFor(owlBottomDataProperty).equals(propertyId)) {
            return owlBottomDataProperty;
        }

        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLDataProperty owlDataProperty: ont.getDataPropertiesInSignature()) {
                if (String.valueOf(owlDataProperty.getIRI().hashCode()).equals(propertyId)){
                    return owlDataProperty;
                }
            }
        }
        throw new NotFoundException("OWLDataProperty", propertyId);
    }

    public String getIdFor(final OWLDataProperty owlDataProperty) {
        return String.valueOf(owlDataProperty.getIRI().hashCode());
    }
}
