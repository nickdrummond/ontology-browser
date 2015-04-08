package org.coode.www.formatter;

import com.google.common.base.Function;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.springframework.format.Formatter;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import javax.annotation.Nullable;
import java.text.ParseException;
import java.util.Locale;

public class OntologyIdFormatter implements Formatter<OWLOntologyID> {

    private final OntologyIRIShortFormProvider sfp = new OntologyIRIShortFormProvider();

    @Override
    public String print(OWLOntologyID owlOntologyID, Locale locale) {
        return owlOntologyID.getDefaultDocumentIRI().transform(new Function<IRI, String>() {

            @Nullable
            @Override
            public String apply(IRI iri) {
                if (iri.toString().equals("http://www.manchester.ac.uk/root.owl")) {
                    return "All ontologies";
                } else {
                    return sfp.getShortForm(iri) + " (" + iri.toString() + ")";
                }
            }
        }).or("Anonymous");
    }

    @Override
    public OWLOntologyID parse(String s, Locale locale) throws ParseException {
        throw new NotImplementedException();
    }
}