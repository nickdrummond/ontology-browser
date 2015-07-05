package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLDatatype;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

public class DatatypesByUsageCloud extends AbstractOWLCloudModel<OWLDatatype>{

    public DatatypesByUsageCloud(Set<OWLOntology> onts, ShortFormProvider renderer, URLScheme urlScheme) {
        super(onts, renderer, urlScheme);
    }

    public Set<OWLDatatype> getEntities() {
        Set<OWLDatatype> entities = new HashSet<OWLDatatype>();
        for (OWLOntology ont : ontologies) {
            entities.addAll(ont.getDatatypesInSignature());
        }
        return entities;
    }

    public String getTitle() {
        return CloudType.datatypeusage.getRendering();
    }

    protected int calculateValue(OWLDatatype entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
