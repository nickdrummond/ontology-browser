package org.coode.www.service;

import com.google.common.base.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import static java.util.Arrays.asList;

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

    public List<Characteristic> getCharacteristics(final OWLObjectProperty owlObjectProperty, final OWLHTMLKit kit) {

        Set<OWLOntology> activeOntologies = kit.getOWLServer().getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getOWLServer().getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        List<Characteristic> characteristics = new ArrayList<>();
        for (Optional<Characteristic> c : asList(
                fac.getAnnotations(owlObjectProperty, activeOntologies, comparator),
                fac.getPropertyCharacteristics(owlObjectProperty, activeOntologies, comparator),
                fac.getDomains(owlObjectProperty, activeOntologies, comparator),
                fac.getRanges(owlObjectProperty, activeOntologies, comparator),
                fac.getInverses(owlObjectProperty, activeOntologies, comparator),
                fac.getEquivalents(owlObjectProperty, activeOntologies, comparator),
                fac.getSupers(owlObjectProperty, activeOntologies, comparator),
                fac.getDisjoints(owlObjectProperty, activeOntologies, comparator),
                fac.getUsage(owlObjectProperty, activeOntologies, comparator)
        )) {
            if (c.isPresent()) {
                characteristics.add(c.get());
            }
        }

        return characteristics;
    }
}
