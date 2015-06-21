package org.coode.www.renderer;

import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;

/**
 * Attempt to provide a better rendering for Anonymous ontologies.
 * TODO FIXME for RDF (eg BBC wildlife) since OWLAPI update always anonymous
 */
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
