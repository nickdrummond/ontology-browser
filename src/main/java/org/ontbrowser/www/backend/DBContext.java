package org.ontbrowser.www.backend;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.configuration.DBConnectionFilter;
import org.ontbrowser.www.kit.impl.EntityIdLookup;
import org.ontbrowser.www.kit.impl.QNameEntityIdLookup;
import org.ontbrowser.www.url.OntologyId;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyID;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import owlapi.DBLabelShortFormProvider;
import owlapi.DBOntologyFactory;
import owlapi.DBStructuralReasoner;
import parser.CanonicalParserFactory;
import renderer.CanonicalRendererFactory;
import tables.ontologies.Ontology;
import tables.ontologies.OntologyImports;
import tables.prefixes.Prefixes;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

import static org.semanticweb.owlapi.vocab.OWLRDFVocabulary.RDFS_LABEL;

@Component
@ConditionalOnProperty(name = "ontology.backend", havingValue = "db")
@RequestScope(proxyMode = ScopedProxyMode.TARGET_CLASS) // for ReadOnlyOntologyControllerAdvice
public class DBContext implements BackendContext {

    // TODO use spring configuration to set this, and allow it to be configured per-ontology in the database
    private static final String LABEL_IRI = System.getenv().getOrDefault("LABEL_IRI", RDFS_LABEL.getIRI().toString());

    // TODO allow multiple ontologies to be stored in the database
    // The root is ontology 1 as long as a single ontology (and its imports) is stored in the database
    private static final int DEFAULT_ONT_ID = 1;

    private final Connection connection; // injected from DBConnectionFilter via request attribute
    private final OWLDataFactory df = new OWLDataFactoryImpl();
    private final Map<String, String> prefixMap;
    private final CanonicalRendererFactory canonicalRendererFactory;
    private final CanonicalParserFactory canonicalParserFactory;

    private OWLOntology rootOntology;
    private Map<OWLOntologyID, OWLReasoner> toldReasoners = new HashMap<>();
    private Map<OWLOntologyID, Integer> ontId2bdId = null;

    public DBContext(HttpServletRequest request) throws SQLException {
        this.connection = (Connection) request.getAttribute(DBConnectionFilter.DB_CONNECTION_ATTR);
        var imports = OntologyImports.getImportsClosure(connection, DEFAULT_ONT_ID);
        // TODO need to fix OWLDB2 - it loses the original prefixes (ontology_prefix table is wrong) - should contain : and util:
        this.prefixMap = Prefixes.getCanonicalMappingsForClosure(connection, imports); // could be static
        this.canonicalRendererFactory = new CanonicalRendererFactory(prefixMap);
        this.canonicalParserFactory = new CanonicalParserFactory(df, prefixMap);
    }

    @Override
    public OWLOntology getRootOntology() {
        if (rootOntology == null) {
            try {
                rootOntology = DBOntologyFactory.openFromDb(connection, DEFAULT_ONT_ID);
            } catch (SQLException e) {
                throw new RuntimeException(e);
            }
        }
        return rootOntology;
    }

    @Override
    public OWLOntology getOntologyFor(String ontId) {
        var root = getRootOntology();
        if (root.getOntologyID().getOntologyIRI().stream().anyMatch((iri) -> iri.equals(IRI.create(ontId)))) {
            return root;
        }
        return root.importsClosure()
                .filter(ont -> OntologyId.getIdForOntology(ont.getOntologyID()).equals(ontId))
                .findFirst().orElseThrow(() -> new RuntimeException("DB Ontology not found: " + ontId));
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
        return new DBLabelShortFormProvider(
                connection,
                canonicalRendererFactory,
                canonicalParserFactory,
                DEFAULT_ONT_ID,
                df.getOWLAnnotationProperty(LABEL_IRI)
        );
    }

    @Override
    public OWLDataFactory getOWLDataFactory() {
        return df;
    }

    @Override
    public OWLReasoner getToldReasoner(OWLOntology ont) {
        return toldReasoners.computeIfAbsent(ont.getOntologyID(), (ontId) -> createReasoner(ont));
    }

    @Override
    public EntityIdLookup lookup() {
        return new QNameEntityIdLookup(prefixMap);
    }

    private OWLReasoner createReasoner(OWLOntology ont) {
        try {
            return DBStructuralReasoner.withClosure(connection, getDBIdForOntology(ont.getOntologyID()), df);
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    private int getDBIdForOntology(OWLOntologyID ontologyID) throws SQLException {
        if (ontId2bdId == null) {
            ontId2bdId = Ontology.owlOntologyIdToId(connection);
        }
        return ontId2bdId.get(ontologyID);
    }
}
