package org.coode.www.cloud;

import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.HashSet;
import java.util.Set;

public class ObjectPropsByUsageCloud extends AbstractOWLCloudModel<OWLObjectProperty>{

    public ObjectPropsByUsageCloud(Set<OWLOntology> onts) {
        super(onts);
    }

    public Set<OWLObjectProperty> getEntities() {
        Set<OWLObjectProperty> objectProperties = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            objectProperties.addAll(ont.getObjectPropertiesInSignature());
        }
        return objectProperties;
    }

    protected int calculateValue(OWLObjectProperty entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
