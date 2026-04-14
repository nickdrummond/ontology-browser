package org.ontbrowser.www.feature.ontologies;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.OntologyCharacteristicsBuilder;
import org.ontbrowser.www.model.paging.With;
import org.ontbrowser.www.util.OWLObjectComparator;
import org.semanticweb.owlapi.metrics.*;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.OWLOntology;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

@Service
public class OWLOntologiesService {

    public String getIdFor(final OWLOntology ontology) {
        return Integer.toString(ontology.getOntologyID().hashCode());
    }

    public List<Characteristic> getCharacteristics(
            final OWLOntology ont,
            final List<With> with,
            final int defaultPageSize,
            final OWLObjectComparator comparator) {
        return new OntologyCharacteristicsBuilder(
                ont,
                with,
                defaultPageSize,
                comparator
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
