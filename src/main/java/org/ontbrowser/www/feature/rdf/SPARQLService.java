package org.ontbrowser.www.feature.rdf;

import org.apache.jena.ontology.OntDocumentManager;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.ontology.OntModelSpec;
import org.apache.jena.query.*;
import org.apache.jena.rdf.model.Model;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.RDFNode;
import org.apache.jena.reasoner.Reasoner;
import org.apache.jena.reasoner.ReasonerRegistry;
import org.apache.jena.sparql.core.DatasetOne;
import org.apache.jena.util.FileManager;
import org.apache.jena.util.LocationMapper;
import org.apache.jena.util.LocatorFile;
import org.ontbrowser.www.kit.OWLHTMLKit;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;

import java.io.File;
import java.util.*;
import java.util.stream.Collectors;

// TODO each ont as a separate graph?
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
        this.dataset = loadInMemoryInfModel(root);
    }

    public Dataset loadInMemoryInfModel(final String root) {
        // TODO make inference optional
        var basicModel = loadMemoryModel(root);

        Reasoner rdfsReasoner = ReasonerRegistry.getRDFSReasoner();
        var rdfsInfModel = ModelFactory.createInfModel(rdfsReasoner, basicModel);

        return DatasetOne.create(rdfsInfModel);
    }

    // TODO STARWARS imports not working!
    public Model loadMemoryModel(final String root) {
        OntModel model;
        long t1 = System.currentTimeMillis();

        File rootFile = new File(root);
        if (rootFile.exists()) {
            model = loadFromFile(rootFile);
        } else {
            model = loadFromClassPath(root); // for demo purposes
        }

        long t2 = System.currentTimeMillis();

        log.info("SPARQL model loaded {} triples, {} imports, in {}ms", model.size(), model.getSubGraphs().size(), t2 - t1);

        return model;
    }

    // NOTE - only works for single file, no imports!!
    private OntModel loadFromClassPath(String root) {
        var res = getClass().getClassLoader().getResource(root);
        if (res != null) {
            try (var in = res.openStream()) {
                OntModel model = ModelFactory.createOntologyModel();
                model.read(in, "base");
                return model;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        } else {
            throw new RuntimeException("Could not find root ontology on classpath: " + root);
        }
    }

    public OntModel loadFromFile(final File root) {
        File baseDir = root.getParentFile();

        FileManager mngr = getFileManager(baseDir);
        OntDocumentManager dm = new OntDocumentManager();
        dm.setFileManager(mngr);
        dm.setProcessImports(true);
        dm.setReadFailureHandler((url, m, e) ->
                log.warn("Failed to load " + url + " " + e.getMessage())
        );

        OntModelSpec spec = new OntModelSpec(OntModelSpec.OWL_MEM);
        spec.setDocumentManager(dm);
        OntModel model = ModelFactory.createOntologyModel(spec);
        model.read(root.toString());
        return model;
    }

    private static FileManager getFileManager(File baseDir) {
        LocationMapper locMapper = new LocationMapper() {
            @Override
            public String altMapping(String uri, String otherwise) {
                String fileName = uri.substring(uri.lastIndexOf('/') + 1);
                File localFile = new File(baseDir, fileName);

                if (localFile.exists()) {
                    String localURI = localFile.getAbsolutePath();
                    log.debug("Mapping {} -> {}", uri, localURI);
                    return super.altMapping(uri, localURI);
                }

                log.warn("No local file found for: {}", uri);
                return super.altMapping(uri, null); // Let Jena know we couldn't map this
            }
        };

        // Set up document manager with our mapper and a file locator
        var mngr = new FileManager(locMapper);
        mngr.addLocator(new LocatorFile(baseDir.getAbsolutePath()));
        return mngr;
    }

    private void listImports(OntModel model) {
        model.getImportModelMaker().listModels().forEachRemaining(mURI -> {
            OntDocumentManager dm = model.getDocumentManager();
            String alt = dm.getFileManager().getLocationMapper().altMapping(mURI);

            OntModel m = dm.getOntology(mURI, OntModelSpec.OWL_MEM);
            if (m != null) {
                log.info("import = " + mURI + " from " + alt);
            } else {
                log.warn("Cannot find model for " + mURI);
            }
        });
    }

    @Override
    public void destroy() {
        this.dataset.close();
    }

    public List<Map<String, OWLObject>> select(String select, OWLOntology ont) {
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
}
