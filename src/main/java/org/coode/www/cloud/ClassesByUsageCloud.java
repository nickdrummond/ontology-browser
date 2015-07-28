package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.HashSet;
import java.util.Set;

public class ClassesByUsageCloud extends AbstractOWLCloudModel<OWLClass>{

    public ClassesByUsageCloud(Set<OWLOntology> onts, ShortFormProvider renderer, URLScheme urlScheme) {
        super(onts, renderer, urlScheme);
    }

    public Set<OWLClass> getEntities() {
        Set<OWLClass> owlClasses = new HashSet<>();
        for (OWLOntology ont : ontologies) {
            owlClasses.addAll(ont.getClassesInSignature());
        }
        return owlClasses;
    }

    public String getTitle() {
        return CloudType.classusage.getRendering();
    }

    protected int calculateValue(OWLClass entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}