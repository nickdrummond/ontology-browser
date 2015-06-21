package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.coode.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class ClassesByUsageCloud extends AbstractOWLCloudModel<OWLClass>{

    private URLScheme urlScheme;

    public ClassesByUsageCloud(OWLHTMLKit kit) {
        super(kit.getOWLServer().getShortFormProvider());
        this.urlScheme = kit.getURLScheme();
        setOntologies(kit.getVisibleOntologies());
    }

    public Set<OWLClass> getEntities() {
        Set<OWLClass> owlClasses = new HashSet<OWLClass>();
        for (OWLOntology ont : getOntologies()) {
            owlClasses.addAll(ont.getClassesInSignature());
        }
        return owlClasses;
    }

    public URL getURL(OWLClass entity) {
        return urlScheme.getURLForOWLObject(entity);
    }

    public String getTitle() {
        return CloudType.classusage.getRendering();
    }

    protected int calculateValue(OWLClass entity) {
        int count = 0;
        for (OWLOntology ont : getOntologies()){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}