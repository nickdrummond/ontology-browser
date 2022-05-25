package org.coode.www.service;

import org.coode.www.exception.NotFoundException;
import org.coode.www.exception.OntServerException;
import org.coode.www.kit.OWLHTMLKit;
import org.coode.www.model.Characteristic;
import org.semanticweb.owlapi.metrics.OWLMetric;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URI;
import java.util.List;
import java.util.Set;

public interface OWLOntologiesService {

    OWLOntology getOntologyFor(String id, OWLHTMLKit kit) throws NotFoundException;

    String getIdFor(OWLOntology ontology);

    String load(URI uri, boolean clear, OWLHTMLKit kit) throws OntServerException;

    OWLOntology getActiveOntology(OWLHTMLKit kit);

    Set<OWLOntology> getOntologies(OWLHTMLKit kit);

    List<Characteristic> getCharacteristics(OWLOntology owlOntology, OWLHTMLKit kit);

    List<OWLMetric<?>> getMetrics(OWLOntology owlOntology);

    void setActiveOntology(OWLOntology ontology, OWLHTMLKit kit) throws OntServerException;
}
