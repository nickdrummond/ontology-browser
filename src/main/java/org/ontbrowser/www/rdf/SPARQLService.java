package org.ontbrowser.www.rdf;

import org.apache.jena.query.*;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.tdb2.TDB2Factory;
import org.apache.jena.tdb2.loader.LoaderFactory;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

public class SPARQLService implements DisposableBean {

    private static final Logger log = LoggerFactory.getLogger(SPARQLService.class);

    public static final String DEFAULT_QUERY = """
            SELECT DISTINCT ?s ?p ?o
            WHERE {
            ?s ?p ?o .
            }
            """.stripLeading();

    private String defaultPrefixes;

    private final Dataset dataset;

    public SPARQLService(String root, String dbLoc) {

        File rootFile = new File(root);
        if (!rootFile.exists()) {
            throw new RuntimeException("Root ontology does not exist: " + rootFile);
        }
        File dir = rootFile.getParentFile();
        String fileExtension = rootFile.getName().substring(rootFile.getName().lastIndexOf(".") + 1);
        var sources = dir.listFiles((file, name) -> name.endsWith(fileExtension));
        if (sources == null) {
            throw new RuntimeException("No files found in " + dir);
        }

        // TODO check if already loaded
        // TODO each ont as a separate graph?

        this.dataset = TDB2Factory.connectDataset(dbLoc);
        var loader = LoaderFactory.parallelLoader(
                dataset.asDatasetGraph(),
                (s, args) -> log.info(String.format(s, args))
        );

        loader.startBulk();
        loader.load(Arrays.stream(sources).map(File::getAbsolutePath).toArray(String[]::new));
        loader.finishBulk();
    }

    @Override
    public void destroy() {
        this.dataset.close();
    }

    public List<Map<String, OWLObject>> select(String select, OWLOntology ont) {
        log.info(select);

        var df = ont.getOWLOntologyManager().getOWLDataFactory();

        Query qry = QueryFactory.create(select);

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
            }
            else {
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
        return "PREFIX " + entry.getKey() + " <" + entry.getValue() + ">";
    }
}
