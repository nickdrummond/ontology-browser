package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.HashSet;
import java.util.Set;

public class ObjectPropsByUsageCloud extends AbstractOWLCloudModel<OWLObjectProperty>{

    public ObjectPropsByUsageCloud(Set<OWLOntology> onts, ShortFormProvider renderer, URLScheme urlScheme) {
        super(onts, renderer, urlScheme);
    }

    public Set<OWLObjectProperty> getEntities() {
        Set<OWLObjectProperty> objectProperties = new HashSet<OWLObjectProperty>();
        for (OWLOntology ont : ontologies) {
            objectProperties.addAll(ont.getObjectPropertiesInSignature());
        }
        return objectProperties;
    }

    public String getTitle() {
        return CloudType.objpropusage.getRendering();
    }

    protected int calculateValue(OWLObjectProperty entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
