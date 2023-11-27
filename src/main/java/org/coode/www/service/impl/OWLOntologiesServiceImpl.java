package org.coode.www.service.impl;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.characteristics.Characteristic;
import org.coode.www.model.characteristics.OntologyCharacteristicsBuilder;
import org.coode.www.service.OWLOntologiesService;
import org.semanticweb.owlapi.metrics.*;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.*;

@Service
public class OWLOntologiesServiceImpl implements OWLOntologiesService {

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
        return new OntologyCharacteristicsBuilder(owlOntology, kit.getComparator()).getCharacteristics();
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
