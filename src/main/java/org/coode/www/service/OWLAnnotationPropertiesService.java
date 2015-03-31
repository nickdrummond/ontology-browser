package org.coode.www.service;

import org.coode.www.kit.OWLHTMLKit;
import org.coode.owl.mngr.HierarchyProvider;
import org.coode.www.exception.NotFoundException;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

@Service
public class OWLAnnotationPropertiesService {

    // TODO need to index the entities by ID
    public OWLAnnotationProperty getOWLAnnotationPropertyFor(String propertyId, OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLAnnotationProperty owlAnnotationProperty: ont.getAnnotationPropertiesInSignature()) {
                if (getIdFor(owlAnnotationProperty).equals(propertyId)){
                    return owlAnnotationProperty;
                }
            }
        }
        throw new NotFoundException("OWLAnnotationProperty", propertyId);
    }

    public String getIdFor(final OWLAnnotationProperty owlAnnotationProperty) {
        return String.valueOf(owlAnnotationProperty.getIRI().hashCode());
    }

    public OWLAnnotationProperty getFirstAnnotationProperty(final OWLHTMLKit kit) throws NotFoundException {
        HierarchyProvider<OWLAnnotationProperty> hp = kit.getOWLServer().getHierarchyProvider(OWLAnnotationProperty.class);
        Set<OWLAnnotationProperty> annotationProperties = hp.getRoots();
        if (!annotationProperties.isEmpty()){
            List<OWLAnnotationProperty> aps = new ArrayList<OWLAnnotationProperty>(annotationProperties);
            Collections.sort(aps, kit.getOWLObjectComparator());
            return aps.get(0);
        }
        throw new NotFoundException("OWLAnnotationProperty", "any");
    }
}
