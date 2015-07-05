package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class DataPropsByUsageCloud extends AbstractOWLCloudModel<OWLDataProperty>{

    public DataPropsByUsageCloud(Set<OWLOntology> onts, ShortFormProvider renderer, URLScheme urlScheme) {
        super(onts, renderer, urlScheme);
    }

    public Set<OWLDataProperty> getEntities() {
        Set<OWLDataProperty> dataProperties = new HashSet<OWLDataProperty>();
        for (OWLOntology ont : ontologies) {
            dataProperties.addAll(ont.getDataPropertiesInSignature());
        }
        return dataProperties;
    }

    public String getTitle() {
        return CloudType.datapropusage.getRendering();
    }

    protected int calculateValue(OWLDataProperty entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
