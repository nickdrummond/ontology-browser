package org.ontbrowser.www.service;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.characteristics.Characteristic;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.metrics.OWLMetric;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.List;
import java.util.Set;

public interface OWLOntologiesService {

    OWLOntology getOntologyFor(String id, OWLHTMLKit kit) throws NotFoundException;

    String getIdFor(OWLOntology ontology);

    List<Characteristic> getCharacteristics(
            OWLOntology owlOntology,
            List<With> with,
            int defaultPageSize,
            OWLHTMLKit kit);

    List<OWLMetric<?>> getMetrics(OWLOntology owlOntology);
}
