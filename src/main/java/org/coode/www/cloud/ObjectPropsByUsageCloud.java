package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class ObjectPropsByUsageCloud extends AbstractOWLCloudModel<OWLObjectProperty>{

    private URLScheme urlScheme;

    public ObjectPropsByUsageCloud(OWLHTMLKit kit) {
        super(kit.getOWLServer().getShortFormProvider());
        this.urlScheme = kit.getURLScheme();
        setOntologies(kit.getVisibleOntologies());
    }

    public Set<OWLObjectProperty> getEntities() {
        Set<OWLObjectProperty> objectProperties = new HashSet<OWLObjectProperty>();
        for (OWLOntology ont : getOntologies()) {
            objectProperties.addAll(ont.getObjectPropertiesInSignature());
        }
        return objectProperties;
    }

    public URL getURL(OWLObjectProperty entity) {
        return urlScheme.getURLForOWLObject(entity);
    }

    public String getTitle() {
        return CloudType.objpropusage.getRendering();
    }

    protected int calculateValue(OWLObjectProperty entity) {
        int count = 0;
        for (OWLOntology ont : getOntologies()){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
