package org.ontbrowser.www.model.cloud;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;

import java.util.HashSet;
import java.util.Set;

public class ClassesByUsageCloud extends AbstractOWLCloudModel<OWLClass>{

    public ClassesByUsageCloud(Set<OWLOntology> onts) {
        super(onts);
    }

    public Set<OWLClass> getEntities() {
        Set<OWLClass> owlClasses = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            owlClasses.addAll(ont.getClassesInSignature());
        }
        return owlClasses;
    }

    protected int calculateValue(OWLClass entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}