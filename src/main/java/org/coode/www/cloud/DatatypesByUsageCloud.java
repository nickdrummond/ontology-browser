package org.coode.www.cloud;

import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.HashSet;
import java.util.Set;

public class DatatypesByUsageCloud extends AbstractOWLCloudModel<OWLDatatype>{

    public DatatypesByUsageCloud(Set<OWLOntology> onts) {
        super(onts);
    }

    public Set<OWLDatatype> getEntities() {
        Set<OWLDatatype> entities = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            entities.addAll(ont.getDatatypesInSignature());
        }
        return entities;
    }

    protected int calculateValue(OWLDatatype entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
