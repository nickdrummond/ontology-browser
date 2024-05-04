package org.ontbrowser.www.model.cloud;

import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.HashSet;
import java.util.Set;

public class DataPropsByUsageCloud extends AbstractOWLCloudModel<OWLDataProperty>{

    public DataPropsByUsageCloud(Set<OWLOntology> onts) {
        super(onts);
    }

    public Set<OWLDataProperty> getEntities() {
        Set<OWLDataProperty> dataProperties = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            dataProperties.addAll(ont.getDataPropertiesInSignature());
        }
        return dataProperties;
    }

    protected int calculateValue(OWLDataProperty entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
