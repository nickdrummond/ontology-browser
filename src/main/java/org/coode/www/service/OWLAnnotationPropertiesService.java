package org.coode.www.service;

import com.google.common.base.Optional;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.exception.NotFoundException;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

import static java.util.Arrays.asList;

@Service
public class OWLAnnotationPropertiesService {

    // TODO need to index the entities by ID
    public OWLAnnotationProperty getOWLAnnotationPropertyFor(final String propertyId, final OWLHTMLKit kit) throws NotFoundException {
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
            Collections.sort(aps, kit.getOWLServer().getComparator());
            return aps.get(0);
        }
        throw new NotFoundException("OWLAnnotationProperty", "any");
    }

    public HierarchyProvider<OWLAnnotationProperty> getHierarchyProvider(final OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLAnnotationProperty.class);
    }

    public List<Characteristic> getCharacteristics(final OWLAnnotationProperty owlAnnotationProperty, final OWLHTMLKit kit) {
        Set<OWLOntology> activeOntologies = kit.getOWLServer().getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getOWLServer().getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        List<Characteristic> characteristics = new ArrayList<>();
        for (Optional<Characteristic> c : asList(
                fac.getAnnotations(owlAnnotationProperty, activeOntologies, comparator),
                fac.getDomains(owlAnnotationProperty, activeOntologies, comparator),
                fac.getRanges(owlAnnotationProperty, activeOntologies, comparator),
                fac.getSupers(owlAnnotationProperty, activeOntologies, comparator),
                fac.getUsage(owlAnnotationProperty, activeOntologies, comparator)
        )) {
            if (c.isPresent()) {
                characteristics.add(c.get());
            }
        }

        return characteristics;
    }
}
