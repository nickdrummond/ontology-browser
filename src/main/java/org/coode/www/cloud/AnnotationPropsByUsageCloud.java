package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.HashSet;
import java.util.Set;

public class AnnotationPropsByUsageCloud extends AbstractOWLCloudModel<OWLAnnotationProperty>{

    public AnnotationPropsByUsageCloud(Set<OWLOntology> onts, ShortFormProvider renderer, URLScheme urlScheme) {
        super(onts, renderer, urlScheme);
    }

    public Set<OWLAnnotationProperty> getEntities() {
        Set<OWLAnnotationProperty> objectProperties = new HashSet<OWLAnnotationProperty>();
        for (OWLOntology ont : ontologies) {
            objectProperties.addAll(ont.getAnnotationPropertiesInSignature());
        }
        return objectProperties;
    }

    public String getTitle() {
        return CloudType.objpropusage.getRendering();
    }

    protected int calculateValue(OWLAnnotationProperty entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}