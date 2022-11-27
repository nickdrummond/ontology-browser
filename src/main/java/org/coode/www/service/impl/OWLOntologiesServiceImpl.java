package org.coode.www.service.impl;

import java.util.Optional;
import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.coode.www.model.CharacteristicsFactory;
import org.coode.www.service.OWLOntologiesService;
import org.semanticweb.owlapi.io.UnparsableOntologyException;
import org.semanticweb.owlapi.metrics.*;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.util.*;

import static java.util.Arrays.asList;

@Service
public class OWLOntologiesServiceImpl implements OWLOntologiesService {

    private static Logger logger = LoggerFactory.getLogger(OWLOntologiesService.class);

    @Override public OWLOntology getOntologyFor(final String id, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getOntologies()){
            if (getIdFor(ont).equals(id)){
                return ont;
            }
        }
        throw new NotFoundException("Ontology", id);
    }

    @Override public String getIdFor(final OWLOntology ontology) {
        return String.valueOf(ontology.getOntologyID().hashCode());
    }

    @Override public OWLOntology getActiveOntology(final OWLHTMLKit kit) {
        return kit.getActiveOntology();
    }

    @Override public Set<OWLOntology> getOntologies(final OWLHTMLKit kit) {
        return kit.getOntologies();
    }

    @Override public List<Characteristic> getCharacteristics(final OWLOntology owlOntology, final OWLHTMLKit kit) {
        Comparator<OWLObject> comparator = kit.getComparator();

        CharacteristicsFactory fac = new CharacteristicsFactory();

        List<Characteristic> characteristics = new ArrayList<>();
        for (Optional<Characteristic> c : asList(
                fac.getAnnotations(owlOntology, comparator),
                fac.getImports(owlOntology, comparator),
                fac.getGeneralClassAxioms(owlOntology, comparator)
        )) {
            c.ifPresent(characteristics::add);
        }
        return characteristics;
    }

    @Override
    public List<OWLMetric<?>> getMetrics(OWLOntology owlOntology) {
        List<OWLMetric<?>> metrics = new ArrayList<>();
        metrics.add(new AxiomCount(owlOntology));
        metrics.add(new LogicalAxiomCount(owlOntology));
        metrics.add(new AxiomTypeMetric(owlOntology, AxiomType.DECLARATION));
        metrics.add(new ReferencedIndividualCount(owlOntology));
        metrics.add(new ReferencedClassCount(owlOntology));
        metrics.add(new ReferencedObjectPropertyCount(owlOntology));
        metrics.add(new ReferencedDataPropertyCount(owlOntology));
        metrics.add(new DLExpressivity(owlOntology));
        return metrics;
    }

}
