package org.coode.www.cloud;

import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.HashSet;
import java.util.Set;

public class AnnotationPropsByUsageCloud extends AbstractOWLCloudModel<OWLAnnotationProperty>{

    public AnnotationPropsByUsageCloud(Set<OWLOntology> onts) {
        super(onts);
    }

    public Set<OWLAnnotationProperty> getEntities() {
        Set<OWLAnnotationProperty> objectProperties = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            objectProperties.addAll(ont.getAnnotationPropertiesInSignature());
        }
        return objectProperties;
    }

    protected int calculateValue(OWLAnnotationProperty entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}