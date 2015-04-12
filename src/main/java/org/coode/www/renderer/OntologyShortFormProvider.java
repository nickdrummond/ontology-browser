package org.coode.www.renderer;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;

public class OntologyShortFormProvider extends OntologyIRIShortFormProvider{

    private static final String ROOT_ONTOLOGY_RENDERING = "All ontologies";

    private final OWLOntology root;

    public OntologyShortFormProvider(final OWLOntology root) {
        this.root = root;
    }

    @Override
    public String getShortForm(final OWLOntology ontology) {
        if (ontology == root){
            return ROOT_ONTOLOGY_RENDERING;
        }
        else if (ontology.isAnonymous()){
            return ontology.getOWLOntologyManager().getOntologyDocumentIRI(ontology).toString();
        }
        else {
            return super.getShortForm(ontology);
        }
    }

}
