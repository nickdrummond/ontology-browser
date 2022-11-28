package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.semanticweb.owlapi.metrics.OWLMetric;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.List;
import java.util.Set;

public interface OWLOntologiesService {

    OWLOntology getOntologyFor(String id, OWLHTMLKit kit) throws NotFoundException;

    String getIdFor(OWLOntology ontology);

    OWLOntology getActiveOntology(OWLHTMLKit kit);

    Set<OWLOntology> getOntologies(OWLHTMLKit kit);

    List<Characteristic> getCharacteristics(OWLOntology owlOntology, OWLHTMLKit kit);

    List<OWLMetric<?>> getMetrics(OWLOntology owlOntology);
}
