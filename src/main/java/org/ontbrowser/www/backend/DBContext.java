package org.ontbrowser.www.backend;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.configuration.DBConnectionFilter;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import owlapi.DBLabelShortFormProvider;
import owlapi.DBOntologyFactory;
import parser.CanonicalParserFactory;
import renderer.CanonicalRendererFactory;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;

import static org.semanticweb.owlapi.vocab.OWLRDFVocabulary.RDFS_LABEL;

@Component
@ConditionalOnProperty(name = "ontology.backend", havingValue = "db")
@RequestScope
public class DBContext implements BackendContext {

    // TODO use spring configuration to set this, and allow it to be configured per-ontology in the database
    private static final String LABEL_IRI = System.getenv().getOrDefault("LABEL_IRI", RDFS_LABEL.getIRI().toString());
    private static final int DEFAULT_ONT_ID = 1; // TODO just to try to get something working, use the number as the ontology ID for now

    private final Connection connection; // injected from DBConnectionFilter via request attribute
    private final OWLDataFactory df = new OWLDataFactoryImpl();

    public DBContext(HttpServletRequest request) {
        this.connection = (Connection) request.getAttribute(DBConnectionFilter.DB_CONNECTION_ATTR);
    }

    @Override
    public OWLOntology getRootOntology() {
        // TODO allow multiple ontologies to be stored in the database
        // The root is ontology 1 as long as a single ontology (and its imports) is stored in the database
        return getOntologyFor(String.valueOf(DEFAULT_ONT_ID)); // TODO just to try to get something working
    }

    @Override
    public OWLOntology getOntologyFor(String ontId) { // TODO just to try to get something working, use the number as the ontology ID for now
        try {
            return DBOntologyFactory.openFromDb(connection, Integer.parseInt(ontId));
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public OntologyIRIShortFormProvider getOntologySFP() {
        // TODO implement this in OWLDB2 to match in-memory - ie look for label
        return new OntologyIRIShortFormProvider();
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        // TODO Maybe cache this prefix map!
        Map<String, String> prefixMap = Map.of();
        var canonicalRendererFactory = new CanonicalRendererFactory(prefixMap);
        var canonicalParserFactory = new CanonicalParserFactory(df, prefixMap);
        return new DBLabelShortFormProvider(
                connection,
                canonicalRendererFactory,
                canonicalParserFactory,
                DEFAULT_ONT_ID,
                df.getOWLAnnotationProperty(LABEL_IRI)
        );
    }
}
