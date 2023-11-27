package org.coode.www.service;

import com.google.common.collect.Sets;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.characteristics.AnnotationPropertyCharacteristicsBuilder;
import org.coode.www.model.characteristics.Characteristic;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class OWLAnnotationPropertiesService {

    public OWLAnnotationProperty getOWLAnnotationPropertyFor(final String propertyId, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getActiveOntologies()){
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

    public List<Characteristic> getCharacteristics(final OWLAnnotationProperty property, final OWLHTMLKit kit) {
        return new AnnotationPropertyCharacteristicsBuilder(property, kit.getActiveOntologies(), kit.getComparator()).getCharacteristics();
    }

    public List<OWLAnnotationProperty> getAnnotationProperties(final OWLOntology activeOntology,
                                                               final Comparator<OWLObject> comparator) {
        Set<OWLAnnotationProperty> props = Sets.newHashSet();
        for (OWLOntology ont : activeOntology.getImportsClosure()) {
            props.addAll(ont.getAnnotationPropertiesInSignature());
        }
        return props.stream().sorted(comparator).collect(Collectors.toList());
    }
}
