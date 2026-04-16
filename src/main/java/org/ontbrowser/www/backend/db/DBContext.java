package org.ontbrowser.www.backend.db;

import jakarta.servlet.http.HttpServletRequest;
import org.ontbrowser.www.backend.BackendContext;
import org.ontbrowser.www.backend.EntityIdLookup;
import org.ontbrowser.www.backend.OWLEntityFinder;
import org.ontbrowser.www.backend.QNameEntityIdLookup;
import org.ontbrowser.www.configuration.DBConnectionFilter;
import org.ontbrowser.www.controller.AppStatus;
import org.ontbrowser.www.feature.cloud.model.CloudModel;
import org.ontbrowser.www.feature.cloud.model.CloudModelFactory;
import org.ontbrowser.www.feature.stats.StatsService;
import org.ontbrowser.www.url.OntologyId;
import org.ontbrowser.www.util.OWLObjectComparator;
import org.semanticweb.owlapi.expression.OWLEntityChecker;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.util.IRIShortFormProvider;
import org.semanticweb.owlapi.util.OntologyIRIShortFormProvider;
import org.semanticweb.owlapi.util.ShortFormProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.ScopedProxyMode;
import org.springframework.stereotype.Component;
import org.springframework.web.context.annotation.RequestScope;
import owlapi.*;
import parser.CanonicalParserFactory;
import renderer.CanonicalRendererFactory;
import tables.ontologies.OntologyImports;
import tables.prefixes.Prefixes;
import uk.ac.manchester.cs.owl.owlapi.OWLDataFactoryImpl;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.Comparator;
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
    private IRIShortFormProvider iriSFP;
    private OWLObjectComparator comparator;
    private OWLEntityFinder finder;
    private OWLEntityChecker entityChecker;

    public DBContext(HttpServletRequest request) throws SQLException {
        this.connection = (Connection) request.getAttribute(DBConnectionFilter.DB_CONNECTION_ATTR);
        var imports = OntologyImports.getImportsClosure(connection, DEFAULT_ONT_ID);
        // TODO cache this?
        // TODO this should be hidden away in OWLDB2
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
        return getShortFormProvider(df.getOWLAnnotationProperty(LABEL_IRI));
    }

    @Override
    public ShortFormProvider getShortFormProvider(OWLAnnotationProperty prop) {
        return new DBLabelShortFormProvider(
                connection,
                canonicalRendererFactory,
                canonicalParserFactory,
                DEFAULT_ONT_ID,
                prop
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

    @Override
    public IRIShortFormProvider getIriShortFormProvider() {
        if (iriSFP == null) {
            iriSFP = iri -> canonicalRendererFactory.getIriRenderer().render(iri);
        }
        return iriSFP;
    }

    @Override
    public Comparator<OWLObject> getComparator() {
        if (comparator == null) {
            comparator = new OWLObjectComparator(getShortFormProvider());
        }
        return comparator;
    }

    @Override
    public OWLEntityFinder getFinder() {
        if (finder == null) {
            var labelSearch = new DBLabelSearch(
                    connection,
                    canonicalRendererFactory,
                    canonicalParserFactory,
                    DEFAULT_ONT_ID,
                    df.getOWLAnnotationProperty(IRI.create(LABEL_IRI))
            );
            finder = new DBEntityFinder(getShortFormProvider(), df, labelSearch);
        }
        return finder;
    }

    @Override
    public OWLEntityChecker getOWLEntityChecker() {
        if (entityChecker == null) {
            entityChecker = new DBEntityChecker(
                    connection,
                    canonicalRendererFactory,
                    canonicalParserFactory,
                    DEFAULT_ONT_ID,
                    df.getOWLAnnotationProperty(LABEL_IRI)
            );
        }
        return entityChecker;
    }


    @Override
    public <T extends OWLEntity> CloudModel<T> getUsageCloud(
            EntityType<T> entityType, OWLOntology ont, Imports imports) {
        try {
            if (ont instanceof DBOntology dbOnt) {
                var dbEntityUsage = new DBEntityUsage(connection, dbOnt);
                Map<T, Integer> usageByEntity = dbEntityUsage
                        .getUsage(entityType, imports, 0);
                return CloudModelFactory.fromPreloadedValues(usageByEntity);
            }
            throw new IllegalArgumentException("Cannot get usage - expected a DBOntology");
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public AppStatus getStatus() {
        return new AppStatus(AppStatus.Status.UP);
    }

    @Override
    public StatsService getStats() {
        return new DBStatsService(this, connection);
    }

    private OWLReasoner createReasoner(OWLOntology ont) {
        try {
            if (ont instanceof DBOntology dbOnt) {
                return DBStructuralReasoner.withClosure(connection, dbOnt);
            }
            throw new IllegalArgumentException("Cannot create reasoner - expected a DBOntology");
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }
}
