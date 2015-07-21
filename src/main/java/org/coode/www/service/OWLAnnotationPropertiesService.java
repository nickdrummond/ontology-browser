package org.coode.www.service;

import com.google.common.base.Optional;
import com.google.common.collect.Sets;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.owl.mngr.OWLEntityFinder;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

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

    public List<OWLAnnotationProperty> getAnnotationProperties(final OWLOntology activeOntology,
                                                               final Comparator<OWLObject> comparator) {
        Set<OWLAnnotationProperty> props = Sets.newHashSet();
        for (OWLOntology ont : activeOntology.getImportsClosure()) {
            props.addAll(ont.getAnnotationPropertiesInSignature());
        }
        return props.stream().sorted(comparator).collect(Collectors.toList());
    }
}
