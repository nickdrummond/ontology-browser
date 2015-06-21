package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class IndividualsByUsageCloud extends AbstractOWLCloudModel<OWLNamedIndividual>{

    private URLScheme urlScheme;

    public IndividualsByUsageCloud(OWLHTMLKit kit) {
        super(kit.getOWLServer().getShortFormProvider());
        this.urlScheme = kit.getURLScheme();
        setOntologies(kit.getVisibleOntologies());
    }

    public Set<OWLNamedIndividual> getEntities() {
        Set<OWLNamedIndividual> entities = new HashSet<OWLNamedIndividual>();
        for (OWLOntology ont : getOntologies()) {
            entities.addAll(ont.getIndividualsInSignature());
        }
        return entities;
    }

    public URL getURL(OWLNamedIndividual entity) {
        return urlScheme.getURLForOWLObject(entity);
    }

    public String getTitle() {
        return CloudType.indusage.getRendering();
    }

    protected int calculateValue(OWLNamedIndividual entity) {
        int count = 0;
        for (OWLOntology ont : getOntologies()){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
