package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLDataProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class DataPropsByUsageCloud extends AbstractOWLCloudModel<OWLDataProperty>{

    private URLScheme urlScheme;

    public DataPropsByUsageCloud(OWLHTMLKit kit) {
        super(kit.getOWLServer().getShortFormProvider());
        this.urlScheme = kit.getURLScheme();
        setOntologies(kit.getVisibleOntologies());
    }

    public Set<OWLDataProperty> getEntities() {
        Set<OWLDataProperty> dataProperties = new HashSet<OWLDataProperty>();
        for (OWLOntology ont : getOntologies()) {
            dataProperties.addAll(ont.getDataPropertiesInSignature());
        }
        return dataProperties;
    }

    public URL getURL(OWLDataProperty entity) {
        return urlScheme.getURLForOWLObject(entity);
    }

    public String getTitle() {
        return CloudType.datapropusage.getRendering();
    }

    protected int calculateValue(OWLDataProperty entity) {
        int count = 0;
        for (OWLOntology ont : getOntologies()){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
