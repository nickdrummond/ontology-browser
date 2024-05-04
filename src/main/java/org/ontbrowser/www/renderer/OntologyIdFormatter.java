package org.ontbrowser.www.renderer;

import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.format.Formatter;
import org.springframework.stereotype.Component;

import javax.annotation.Nonnull;
import java.util.Locale;

@Component
public class OntologyIdFormatter implements Formatter<OWLOntologyID> {

    @Autowired
    private OntologyIRIShortFormProvider sfp;

    @Value("${ontology.root.location}")
    private String rootIri;

    @Override
    public @Nonnull String print(@Nonnull OWLOntologyID owlOntologyID, @Nonnull Locale locale) {
        return owlOntologyID.getDefaultDocumentIRI().map(iri -> {
            if (iri.toString().equals(rootIri)) {
                return "All ontologies";
            } else {
                return sfp.getShortForm(iri) + " (" + iri + ")";
            }
        }).orElse("Anonymous");
    }

    @Override
    public @Nonnull OWLOntologyID parse(@Nonnull String s, @Nonnull Locale locale) {
        throw new RuntimeException("Ouch!");
    }
}