package org.ontbrowser.www.feature.entities;

import org.ontbrowser.www.feature.entities.characteristics.Characteristic;
import org.ontbrowser.www.feature.entities.characteristics.IndividualCharacteristicsBuilder;
import org.ontbrowser.www.model.AxiomWithMetadata;
import org.ontbrowser.www.model.paging.With;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.AxiomAnnotations;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import static org.semanticweb.owlapi.model.AxiomType.CLASS_ASSERTION;

@Service
public class OWLIndividualsService {

    public List<Characteristic> getCharacteristics(final OWLNamedIndividual owlIndividual,
                                                   final OWLOntology ont,
                                                   final Comparator<OWLObject> comparator,
                                                   final List<With> with,
                                                   final int defaultPageSize) {

        return new IndividualCharacteristicsBuilder(owlIndividual, ont, comparator, with, defaultPageSize).getCharacteristics();
    }

    public List<Characteristic> getInferredCharacteristics(
            OWLNamedIndividual owlIndividual,
            OWLReasoner reasoner) {
        OWLOntology reasonerRootOnt = reasoner.getRootOntology();
        OWLOntologyManager mngr = reasonerRootOnt.getOWLOntologyManager();
        OWLDataFactory df = mngr.getOWLDataFactory();
        try {
            IRI inferredIRI = IRI.create("inferred");
            OWLOntology ont = (mngr.contains(inferredIRI)) ?
                    mngr.getOntology(inferredIRI) :
                    mngr.createOntology(inferredIRI);
            List<AxiomWithMetadata> inf = reasonerRootOnt.objectPropertiesInSignature(Imports.INCLUDED)
                    .flatMap(p -> reasoner.objectPropertyValues(owlIndividual, p)
                            .map(obj -> df.getOWLObjectPropertyAssertionAxiom(p, owlIndividual, obj))
                            .filter(ax -> !reasonerRootOnt.containsAxiom(ax, Imports.INCLUDED, AxiomAnnotations.IGNORE_AXIOM_ANNOTATIONS))
                            .map(ax -> new AxiomWithMetadata("Inferred Relations", ax, ax, ont)))
                    .toList();
            return List.of(new Characteristic(owlIndividual, "Inferred Relations", inf));
        } catch (OWLOntologyCreationException e) {
            throw new RuntimeException(e);
        }
    }

    public Set<OWLClass> getNamedTypes(OWLNamedIndividual ind, OWLOntology ont) {
        return ont.getAxioms(ind, Imports.INCLUDED).stream()
                .filter(ax -> ax.isOfType(CLASS_ASSERTION))
                .map(ax -> ((OWLClassAssertionAxiom)ax).getClassExpression())
                .filter(OWLClassExpression::isNamed)
                .map(OWLClassExpression::asOWLClass)
                .collect(Collectors.toSet());
    }
}
