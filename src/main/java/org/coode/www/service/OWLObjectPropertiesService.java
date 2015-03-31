package org.coode.www.service;

import org.coode.html.OWLHTMLKit;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

@Service
public class OWLObjectPropertiesService {

    // TODO need to index the entities by ID
    public OWLObjectProperty getOWLObjectPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        OWLDataFactory df = kit.getOWLServer().getOWLOntologyManager().getOWLDataFactory();

        OWLObjectProperty owlTopObjectProperty = df.getOWLTopObjectProperty();
        if (getIdFor(owlTopObjectProperty).equals(propertyId)) {
            return owlTopObjectProperty;
        }

        OWLObjectProperty owlBottomObjectProperty = df.getOWLBottomObjectProperty();
        if (getIdFor(owlBottomObjectProperty).equals(propertyId)) {
            return owlBottomObjectProperty;
        }

        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLObjectProperty owlObjectProperty: ont.getObjectPropertiesInSignature()) {
                if (String.valueOf(owlObjectProperty.getIRI().hashCode()).equals(propertyId)){
                    return owlObjectProperty;
                }
            }
        }
        throw new NotFoundException("OWLObjectProperty", propertyId);
    }

    public String getIdFor(final OWLObjectProperty owlObjectProperty) {
        return String.valueOf(owlObjectProperty.getIRI().hashCode());
    }
}
