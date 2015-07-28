package org.coode.www.cloud;

import org.coode.html.url.URLScheme;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.HashSet;
import java.util.Set;

public class IndividualsByUsageCloud extends AbstractOWLCloudModel<OWLNamedIndividual>{

    public IndividualsByUsageCloud(Set<OWLOntology> onts, ShortFormProvider renderer, URLScheme urlScheme) {
        super(onts, renderer, urlScheme);
    }

    public Set<OWLNamedIndividual> getEntities() {
        Set<OWLNamedIndividual> entities = new HashSet<OWLNamedIndividual>();
        for (OWLOntology ont : ontologies) {
            entities.addAll(ont.getIndividualsInSignature());
        }
        return entities;
    }

    public String getTitle() {
        return CloudType.indusage.getRendering();
    }

    protected int calculateValue(OWLNamedIndividual entity) {
        int count = 0;
        for (OWLOntology ont : ontologies){
            count += ont.getReferencingAxioms(entity).size();
        }
        return count;
    }
}
