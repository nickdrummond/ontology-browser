package org.ontbrowser.www.model.cloud;

import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.HashSet;
import java.util.Set;

public class IndividualsByUsageCloud extends AbstractOWLCloudModel<OWLNamedIndividual>{

    public IndividualsByUsageCloud(Set<OWLOntology> onts) {
        super(onts);
    }

    public Set<OWLNamedIndividual> getEntities() {
        Set<OWLNamedIndividual> entities = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            entities.addAll(ont.getIndividualsInSignature());
        }
        return entities;
    }

    protected int calculateValue(OWLNamedIndividual entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
