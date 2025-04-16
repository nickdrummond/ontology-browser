package org.ontbrowser.www.rdf;

import org.apache.jena.query.*;
import org.apache.jena.tdb2.TDB2Factory;
import org.apache.jena.tdb2.loader.LoaderFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.DisposableBean;

import java.io.File;
import java.util.Arrays;

public class FusekiService implements DisposableBean {

    private static final Logger log = LoggerFactory.getLogger(FusekiService.class);

    private final Dataset dataset;

    public FusekiService(String root, String dbLoc) {

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

        query("""
                PREFIX owl: <http://www.w3.org/2002/07/owl#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                PREFIX sw: <https://nickdrummond.github.io/star-wars-ontology/ontologies#>
                
                SELECT DISTINCT ?label ?rel ?cl
                WHERE {
                ?s a owl:NamedIndividual ;
                   rdfs:label ?label ;
                   sw:during sw:Battle_of_Yavin ;
                   ?r ?c .
                ?r rdfs:label ?rel .
                ?c rdfs:label ?cl .
                }
                """
        );
    }

    private void query(String sparql) {
        this.dataset.begin(ReadWrite.READ);

        Query qry = QueryFactory.create(sparql);
        QueryExecution qe = QueryExecutionFactory.create(qry, this.dataset.getDefaultModel());
        ResultSet rs = qe.execSelect();

        rs.forEachRemaining(sol -> {
            log.warn("Result:");
            sol.varNames().forEachRemaining(varName ->
                    log.info("    {} {}", varName, sol.get(varName))
            );
        });

        this.dataset.end();
    }

    @Override
    public void destroy() throws Exception {
        this.dataset.close();
    }
}
