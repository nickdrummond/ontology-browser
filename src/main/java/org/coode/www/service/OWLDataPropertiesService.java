package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.coode.www.renderer.UsageVisibilityVisitor;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

import static java.util.Arrays.asList;

@Service
public class OWLDataPropertiesService {

    // TODO need to index the entities by ID
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

    public List<Characteristic> getCharacteristics(final OWLDataProperty owlDataProperty, final OWLHTMLKit kit) {

        Set<OWLOntology> activeOntologies = kit.getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        List<Characteristic> characteristics = new ArrayList<>();
        for (Optional<Characteristic> c : asList(
                fac.getAnnotations(owlDataProperty, activeOntologies, comparator),
                fac.getPropertyCharacteristics(owlDataProperty, activeOntologies, comparator),
                fac.getDomains(owlDataProperty, activeOntologies, comparator),
                fac.getRanges(owlDataProperty, activeOntologies, comparator),
                fac.getEquivalents(owlDataProperty, activeOntologies, comparator),
                fac.getSupers(owlDataProperty, activeOntologies, comparator),
                fac.getDisjoints(owlDataProperty, activeOntologies, comparator),
                fac.getUsage(owlDataProperty, activeOntologies, comparator, new UsageVisibilityVisitor())
        )) {
            c.ifPresent(characteristics::add);
        }

        return characteristics;
    }
}
