package org.ontbrowser.www.renderer;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;

/**
 * Check if a label annotation exists on the ontology.
 */
public class OntologyShortFormProvider extends OntologyIRIShortFormProvider{

    private final IRI labelIRI;

    public OntologyShortFormProvider(final IRI labelIRI) {
        this.labelIRI = labelIRI;
    }

    @Override
    public String getShortForm(OWLOntology ontology) {
        var maybeLabel = ontology.getAnnotations().stream()
                .filter(annot -> annot.getProperty().getIRI().equals(labelIRI))
                .filter(annot -> annot.getValue().isLiteral())
                .map(annot -> annot.getValue().asLiteral().get())
                .map(OWLLiteral::getLiteral)
                .findFirst();
        return maybeLabel.orElseGet(() -> fromURI(ontology));
    }

    private String fromURI(OWLOntology ontology) {
        if (ontology.isAnonymous()){
            return ontology.getOWLOntologyManager().getOntologyDocumentIRI(ontology).getFragment();
        }
        return super.getShortForm(ontology);
    }
}
