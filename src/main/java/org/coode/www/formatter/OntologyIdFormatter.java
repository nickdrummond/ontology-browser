package org.coode.www.formatter;

import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.format.Formatter;
import org.springframework.stereotype.Component;

import java.text.ParseException;
import java.util.Locale;

@Component
public class OntologyIdFormatter implements Formatter<OWLOntologyID> {

    @Autowired
    private OntologyIRIShortFormProvider sfp;

    @Value("${ontology.root.iri}")
    private String rootIri;

    @Override
    public String print(OWLOntologyID owlOntologyID, Locale locale) {
        return owlOntologyID.getDefaultDocumentIRI().map(iri -> {
            if (iri.toString().equals(rootIri)) {
                return "All ontologies";
            } else {
                return sfp.getShortForm(iri) + " (" + iri.toString() + ")";
            }
        }).orElse("Anonymous");
    }

    @Override
    public OWLOntologyID parse(String s, Locale locale) throws ParseException {
        throw new RuntimeException("Ouch!");
    }
}