package org.coode.www.service;

import org.coode.owl.mngr.HierarchyProvider;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.*;
import org.springframework.stereotype.Service;

import java.util.Set;

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

    public HierarchyProvider<OWLDataProperty> getHierarchyProvider(final OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLDataProperty.class);
    }
}
