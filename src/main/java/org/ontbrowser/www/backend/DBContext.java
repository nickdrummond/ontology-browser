package org.ontbrowser.www.backend;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.configuration.DBConnectionFilter;
import org.ontbrowser.www.url.OntologyId;
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

    // TODO allow multiple ontologies to be stored in the database
    // The root is ontology 1 as long as a single ontology (and its imports) is stored in the database
    private static final int DEFAULT_ONT_ID = 1;

    private final Connection connection; // injected from DBConnectionFilter via request attribute
    private final OWLDataFactory df = new OWLDataFactoryImpl();
    private OWLOntology rootOntology;

    public DBContext(HttpServletRequest request) {
        this.connection = (Connection) request.getAttribute(DBConnectionFilter.DB_CONNECTION_ATTR);
    }

    @Override
    public OWLOntology getRootOntology() {
        if (rootOntology == null) {
            try {
                return DBOntologyFactory.openFromDb(connection, DEFAULT_ONT_ID);
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }
        return rootOntology;
    }

    @Override
    public OWLOntology getOntologyFor(String ontId) {
        return getRootOntology().importsClosure()
                .filter(ont -> OntologyId.getIdForOntology(ont.getOntologyID()).equals(ontId))
                .findFirst().orElseThrow(() -> new RuntimeException("Ontology not found: " + ontId));
    }

    @Override
    public OntologyIRIShortFormProvider getOntologySFP() {
        // TODO implement this in OWLDB2 to match in-memory - ie look for label
        return new OntologyIRIShortFormProvider();
    }

    @Override
    public ShortFormProvider getShortFormProvider() {
        // TODO Maybe cache this prefix map!
        // TODO this should be hidden away in OWLDB2
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
