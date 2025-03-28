package org.ontbrowser.www.feature.ontologies;

import org.ontbrowser.www.exception.NotFoundException;
import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.OntologyCharacteristicsBuilder;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.metrics.*;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class OWLOntologiesService {

    public OWLOntology getOntologyFor(final String id, final OWLHTMLKit kit) throws NotFoundException {
        for (OWLOntology ont : kit.getOntologies()){
            if (getIdFor(ont).equals(id)){
                return ont;
            }
        }
        throw new NotFoundException("Ontology", id);
    }

    public String getIdFor(final OWLOntology ontology) {
        return String.valueOf(ontology.getOntologyID().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLOntology ont,
            final List<With> with,
            final int defaultPageSize,
            final OWLHTMLKit kit) {
        return new OntologyCharacteristicsBuilder(
                ont,
                with,
                defaultPageSize,
                kit.getComparator()
        ).getCharacteristics();
    }

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
