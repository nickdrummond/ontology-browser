package org.coode.www.service;

import com.google.common.base.Optional;
import org.coode.owl.hierarchy.HierarchyProvider;
import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.search.EntitySearcher;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.stereotype.Service;

import java.util.*;

import static java.util.Arrays.asList;

@Service
public class OWLIndividualsService {

    // TODO need to index the entities by ID
    public OWLNamedIndividual getOWLIndividualFor(final String propertyId, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getOWLServer().getActiveOntologies()){
            for (OWLNamedIndividual owlIndividual: ont.getIndividualsInSignature()) {
                if (getIdFor(owlIndividual).equals(propertyId)) {
                    return owlIndividual;
                }
            }
        }
        throw new NotFoundException("OWLIndividual", propertyId);
    }

    public String getIdFor(final OWLIndividual owlIndividual) {
        if (owlIndividual.isNamed()){
            return String.valueOf(owlIndividual.asOWLNamedIndividual().getIRI().hashCode());
        }
        else {
            return String.valueOf(owlIndividual.asOWLAnonymousIndividual().getID().hashCode());
        }
    }

    public HierarchyProvider<OWLNamedIndividual> getHierarchyProvider(final OWLHTMLKit kit) {
        return kit.getOWLServer().getHierarchyProvider(OWLNamedIndividual.class);
    }

    public List<Characteristic> getCharacteristics(OWLNamedIndividual owlIndividual, OWLHTMLKit kit) {
        Set<OWLOntology> activeOntologies = kit.getOWLServer().getActiveOntologies();
        Comparator<OWLObject> comparator = kit.getOWLServer().getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        List<Characteristic> characteristics = new ArrayList<>();
        for (Optional<Characteristic> c : asList(
                fac.getTypes(owlIndividual, activeOntologies, comparator),
                fac.getSameAs(owlIndividual, activeOntologies, comparator),
                fac.getDifferentFrom(owlIndividual, activeOntologies, comparator)
        )) {
            if (c.isPresent()) {
                characteristics.add(c.get());
            }
        }

        ShortFormProvider shortFormProvider = kit.getOWLServer().getShortFormProvider();

        characteristics.addAll(fac.getAnnotationCharacterists (owlIndividual, activeOntologies, comparator, shortFormProvider));
        characteristics.addAll(fac.getDataPropertyAssertions  (owlIndividual, activeOntologies, comparator, shortFormProvider));
        characteristics.addAll(fac.getObjectPropertyAssertions(owlIndividual, activeOntologies, comparator, shortFormProvider));

        // TODO negative property assertions

        return characteristics;
    }

    public OWLNamedIndividual getFirstIndividual(final OWLHTMLKit kit) throws NotFoundException {

        HierarchyProvider hp = kit.getOWLServer().getHierarchyProvider(OWLNamedIndividual.class);

        Set roots = hp.getRoots();
        if (!roots.isEmpty()){
            List<OWLClass> aps = new ArrayList<>(roots);
            Collections.sort(aps, kit.getOWLServer().getComparator());
            List<OWLNamedIndividual> inds = new ArrayList<>(hp.getChildren(aps.get(0)));
            Collections.sort(inds, kit.getOWLServer().getComparator());
            return inds.get(0);
        }
        throw new NotFoundException("OWLAnnotationProperty", "any");
    }
}
