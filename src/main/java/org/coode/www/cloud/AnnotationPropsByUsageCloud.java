package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class AnnotationPropsByUsageCloud extends AbstractOWLCloudModel<OWLAnnotationProperty>{

    private URLScheme urlScheme;

    public AnnotationPropsByUsageCloud(OWLHTMLKit kit) {
        super(kit.getOWLServer().getShortFormProvider());
        this.urlScheme = kit.getURLScheme();
        setOntologies(kit.getVisibleOntologies());
    }

    public Set<OWLAnnotationProperty> getEntities() {
        Set<OWLAnnotationProperty> objectProperties = new HashSet<OWLAnnotationProperty>();
        for (OWLOntology ont : getOntologies()) {
            objectProperties.addAll(ont.getAnnotationPropertiesInSignature());
        }
        return objectProperties;
    }

    public URL getURL(OWLAnnotationProperty entity) {
        return urlScheme.getURLForOWLObject(entity);
    }

    public String getTitle() {
        return CloudType.objpropusage.getRendering();
    }

    protected int calculateValue(OWLAnnotationProperty entity) {
        int count = 0;
        for (OWLOntology ont : getOntologies()){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}