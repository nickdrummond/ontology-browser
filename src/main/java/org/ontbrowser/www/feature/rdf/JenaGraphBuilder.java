package org.ontbrowser.www.feature.rdf;

import org.apache.jena.rdf.model.*;
import org.apache.jena.vocabulary.RDF;
import org.ontbrowser.www.feature.graph.Graph;
import org.ontbrowser.www.feature.graph.GraphBuilder;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Set;
import java.util.stream.Collectors;

public class JenaGraphBuilder implements GraphBuilder {
    private static final Logger log = LoggerFactory.getLogger(JenaGraphBuilder.class);
    public static final Resource RDF_RESOURCE = ResourceFactory.createResource("http://www.w3.org/2000/01/rdf-schema#Resource");

    private final Model model;
    private final OWLDataFactory df;

    public JenaGraphBuilder(Model model, OWLDataFactory df) {
        this.model = model;
        this.df = df;
    }

    @Override
    public Graph build() {

        // Get a stream of statements from the Jena model
        Set<Graph.Edge> edges = model.listStatements().toList().stream()
                .filter(this::filterStatement) // Filter subjects
                .map(stmt -> new Graph.Edge(
                        translateSubject(stmt.getSubject()),
                        translatePredicate(stmt.getPredicate()),
                        translateObject(stmt.getObject())
                )).collect(Collectors.toSet());

        return new Graph(edges);
    }

    private boolean filterStatement(Statement stmt) {
        if (stmt.getPredicate().equals(RDF.type)) {
            if (isVocab(stmt.getObject())) {
                return false;
            }
        }
        return true;
    }

    // Is RDF or OWL vocabulary
    private boolean isVocab(RDFNode node) {
        if (node.isURIResource()) {
            var iri = node.toString();
            return (iri.startsWith("http://www.w3.org/2000/01/rdf-schema#") ||
                    iri.startsWith("http://www.w3.org/1999/02/22-rdf-syntax-ns#") ||
                    iri.startsWith("http://www.w3.org/2002/07/owl#"));
        }
        return false;
    }

    private OWLObject translateSubject(Resource resource) {
        if (resource.isURIResource()) {
            return df.getOWLNamedIndividual(resource.getURI());
        }
        log.info("Translating blank node subject: {} : {}", resource, resource.isAnon());
        return df.getOWLAnonymousIndividual(resource.toString());
    }

    private OWLObject translateObject(RDFNode object) {
        if (object.isURIResource()) {
            return df.getOWLNamedIndividual(object.asNode().getURI());
        } else if (object.isLiteral()) {
            return df.getOWLLiteral(object.asLiteral().getLexicalForm());
        }
        return df.getOWLAnonymousIndividual(object.toString());
    }

    private OWLProperty translatePredicate(Property property) {
        return df.getOWLObjectProperty(property.toString());
    }
}
