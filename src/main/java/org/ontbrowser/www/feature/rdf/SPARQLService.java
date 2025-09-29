package org.ontbrowser.www.feature.rdf;

import jakarta.annotation.PostConstruct;
import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.reasoner.ReasonerRegistry;
import org.apache.jena.sparql.core.DatasetOne;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.ontbrowser.www.kit.event.RestartEvent;
import org.ontbrowser.www.kit.impl.RestartableKit;
import org.semanticweb.owlapi.formats.TurtleDocumentFormat;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.context.annotation.Profile;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.stream.Collectors;

// TODO each ont as a separate graph?
@Profile("rdf")
@Service
public class SPARQLService implements DisposableBean {

    private static final Logger log = LoggerFactory.getLogger(SPARQLService.class);

    public static final String DEFAULT_QUERY = """
            SELECT DISTINCT ?s ?p ?o
            WHERE {
            ?s ?p ?o .
            }
            """.stripLeading();

    private RestartableKit kit;

    private String defaultPrefixes;

    private Dataset dataset;

    public SPARQLService(RestartableKit kit) {
        this.kit = kit;
    }

    @PostConstruct
    public void init() {
        try {
            this.dataset = loadInMemoryInfModel();
        } catch (Exception e) {
            // Do not kill the browser if jena fails to load the ontology
            log.error("SPARQL Failed to load ontology: {}", e.getMessage());
            this.dataset = null; // or set to a fallback Dataset if desired
        }
    }

    public Dataset loadInMemoryInfModel() {
        // TODO make inference optional
        var basicModel = loadMemoryModel();

        var rdfsReasoner = ReasonerRegistry.getRDFSReasoner();
        var rdfsInfModel = ModelFactory.createInfModel(rdfsReasoner, basicModel);

        return DatasetOne.create(rdfsInfModel);
    }

    public Model loadMemoryModel() {
        OntModel model;
        long t1 = System.currentTimeMillis();

        model = toJenaModelWithImports(kit.getOWLOntologyManager());

        long t2 = System.currentTimeMillis();

        log.info("SPARQL model loaded {} triples, {} imports, in {}ms", model.size(), model.getSubGraphs().size(), t2 - t1);

        return model;
    }

    @Override
    public void destroy() {
        this.dataset.close();
    }

    public List<Map<String, OWLObject>> select(String select, OWLOntology ont) {
        if (this.dataset == null) {
            throw new IllegalStateException("No RDF dataset loaded");
        }

        var df = ont.getOWLOntologyManager().getOWLDataFactory();

        var qry = QueryFactory.create(select);

        this.dataset.begin(ReadWrite.READ);

        try (QueryExecution qe = QueryExecutionFactory.create(qry, this.dataset.getDefaultModel())) {
            var results = qe.execSelect();
            var variablesInOrder = results.getResultVars();
            List<Map<String, OWLObject>> r = new ArrayList<>();
            results.forEachRemaining(sol -> {
                Map<String, OWLObject> map = new LinkedHashMap<>();
                r.add(map);
                for (var variable : variablesInOrder) {
                    RDFNode rdfNode = sol.get(variable);
                    if (rdfNode != null) { // can have no binding if vars are not in the query
                        map.put(variable, toOWL(rdfNode, df, ont));
                    } else {
                        map.put(variable, null); // empty literal for no binding
                    }
                }
            });
            return r;
        } catch (QueryParseException e) {
            throw e;
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            this.dataset.end();
        }
    }

    private OWLObject toOWL(RDFNode rdfNode, OWLDataFactory df, OWLOntology ont) {
        if (rdfNode.isURIResource()) {
            IRI iri = IRI.create(rdfNode.asResource().getURI());
            var entity = getEntity(iri, ont, df);
            if (entity.isPresent()) {
                return entity.get();
            } else {
                return df.getOWLLiteral(iri.getFragment());
            }
        } else if (rdfNode.isLiteral()) {
            var literal = rdfNode.asLiteral();
            var value = literal.getValue().toString();
            if (literal.getLanguage() != null) {
                return df.getOWLLiteral(value, literal.getLanguage());
            }
            if (literal.getDatatypeURI() != null) {
                return df.getOWLLiteral(value, getDatatype(IRI.create(literal.getDatatypeURI()), ont, df));
            }
            return df.getOWLLiteral(value); // just a string
        }
        return df.getOWLLiteral(rdfNode.toString()); // blank node
    }

    private Optional<OWLEntity> getEntity(IRI iri, OWLOntology ont, OWLDataFactory df) {
        var matchingEntities = getEntities(iri, ont, df);
        if (matchingEntities.isEmpty()) {
            return Optional.empty();
        }
        return Optional.of(matchingEntities.iterator().next()); // First
    }

    public Set<OWLEntity> getEntities(IRI iri, OWLOntology ont, OWLDataFactory df) {
        return EntityType.values().stream()
                .map(t -> {
                    OWLEntity e = df.getOWLEntity(t, iri);
                    if (ont.containsEntityInSignature(e, Imports.INCLUDED)) {
                        return e;
                    }
                    return null;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toSet());
    }

    public OWLDatatype getDatatype(IRI iri, OWLOntology ont, OWLDataFactory df) {
        if (ont.containsDatatypeInSignature(iri, Imports.INCLUDED)) {
            return df.getOWLDatatype(iri);
        }
        throw new RuntimeException("No datatype found for " + iri);
    }


    public String getDefaultPrefixes(OWLHTMLKit kit) {
        if (this.defaultPrefixes == null) {
            var sparqlPrefixes = kit.getPrefixes().entrySet().stream()
                    .map(SPARQLService::toSparqlPrefix)
                    .toList();
            this.defaultPrefixes = String.join("\n", sparqlPrefixes);
        }
        return this.defaultPrefixes;
    }

    private static String toSparqlPrefix(Map.Entry<String, String> entry) {
        return "PREFIX " + entry.getKey() + ": <" + entry.getValue() + ">";
    }

    @EventListener
    public void onRestart(RestartEvent event) {
        log.info("SPARQLService received RestartEvent, reloading ontology...");
        try {
            this.dataset.close();
            init();
        } catch (Exception e) {
            log.error("SPARQL Failed to reload ontology on restart: {}", e.getMessage());
            this.dataset = null;
        }
    }

    /**
     * Converts all ontologies loaded in the OWLOntologyManager (including imports) to a single Jena OntModel using Turtle format.
     */
    public static OntModel toJenaModelWithImports(OWLOntologyManager manager) {
        try {
            var dm = new OntDocumentManager();
            dm.setProcessImports(false); // We handle imports ourselves
            dm.setReadFailureHandler((url, m, e) ->
                    log.warn("Failed to load " + url + " " + e.getMessage())
            );

            var spec = new OntModelSpec(OntModelSpec.OWL_MEM);
            spec.setDocumentManager(dm);
            var jenaModel = ModelFactory.createOntologyModel(spec);
            for (OWLOntology ont : manager.getOntologies()) {
                var out = new ByteArrayOutputStream();
                var format = new TurtleDocumentFormat();
                manager.saveOntology(ont, format, out);
                String base = format.getDefaultPrefix(); // Get base from Turtle format
                jenaModel.read(new ByteArrayInputStream(out.toByteArray()), base, "TTL");
            }
            return jenaModel;
        } catch (Exception e) {
            throw new RuntimeException("Failed to convert OWLOntology (with imports) to Jena OntModel", e);
        }
    }
}
