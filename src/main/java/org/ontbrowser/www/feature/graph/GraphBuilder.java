package org.ontbrowser.www.feature.graph;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

public class GraphBuilder {

    private static final Logger log = LoggerFactory.getLogger(GraphBuilder.class);

    private final GraphDescriptor descr;
    private final OWLOntology ont;
    private final OWLDataFactory df;
    private final Set<Graph.Edge> edges = new HashSet<>();
    private boolean built = false;
    private final Set<OWLObject> renderedObjects = new HashSet<>();

    public GraphBuilder(GraphDescriptor descr) {
        this.descr = descr;
        this.ont = descr.getOntology();
        this.df = ont.getOWLOntologyManager().getOWLDataFactory();
    }

    // Add arbitrary edges
    public GraphBuilder add(Graph.Edge edge) {
        this.edges.add(edge);
        return this;
    }

    public Graph build() {
        if (!built) {
            descr.getOwlObjects().forEach(owlObject -> build(owlObject, 0));
            built = true;
        }
        return new Graph(edges);
    }

    private void build(OWLObject owlObject, int depth) {

        if (depth > descr.getMaxDepth()) {
            return;
        }

        if (renderedObjects.contains(owlObject)) {
            return;
        }

        renderedObjects.add(owlObject);

        if (owlObject instanceof OWLEntity entity) {
            buildEntity(entity, depth);
        }
//        else {
//            buildExpression(owlObject, depth);
//        }
    }

    private void buildEntity(OWLEntity entity, int depth) {
        ont.referencingAxioms(entity, Imports.INCLUDED).forEach(ax -> ax.accept(new OWLAxiomVisitor() {
            @Override
            public void visit(@Nonnull OWLObjectPropertyAssertionAxiom axiom) {
                buildRelation(axiom, entity, depth);
            }

            @Override
            public void visit(@Nonnull OWLDataPropertyAssertionAxiom axiom) {
                buildDataRelation(axiom);
            }

            @Override
            public void visit(OWLClassAssertionAxiom axiom) {
                if (axiom.getIndividual().equals(entity)) {
                    handleClassExpressionInd(entity, axiom.getClassExpression(), depth);
                }
            }

            @Override
            public void doDefault(Object object) {
                if (!(object instanceof OWLDeclarationAxiom)) {
                    log.debug("Ignoring axiom {} ", object);
                }
            }
        }));
    }

    private void buildDataRelation(OWLDataPropertyAssertionAxiom axiom) {
        if (axiom.getProperty() instanceof OWLDataProperty dataProperty) {
            if (descr.isAllowedProperty(dataProperty)) {
                var newEdge = new Graph.Edge(axiom.getSubject(), dataProperty, axiom.getObject());
                edges.add(newEdge);
            }
        }
    }

    private void buildRelation(OWLObjectPropertyAssertionAxiom axiom, OWLEntity entity, int depth) {
        // properties are the focus
        if (entity instanceof OWLObjectProperty) {
            if (axiom.getProperty() instanceof OWLObjectProperty objectProperty) {
                var newEdge = new Graph.Edge(axiom.getSubject(), objectProperty, axiom.getObject());
                if (edges.add(newEdge)) {
                    build(axiom.getSubject(), descr.isFollowProperty(objectProperty) ? depth : depth + 1);
                    build(axiom.getObject(), descr.isFollowProperty(objectProperty) ? depth : depth + 1);
                }
            }
            return;
        }

        // Only named props for now
        if (axiom.getProperty() instanceof OWLObjectProperty objectProperty) {

            Predicate<OWLProperty> isAllowed = axiom.getSubject().equals(entity)
                    ? descr::isAllowedProperty
                    : descr::isAllowedInverseProperty;

            boolean added = addIfFilterAllows(
                    axiom.getSubject(),
                    objectProperty,
                    axiom.getObject(),
                    isAllowed);

            if (added) {
                build(axiom.getObject(), descr.isFollowProperty(objectProperty) ? depth : depth + 1);
            }
        }
        else {
            log.debug("axiom ignored as anon {}", axiom);
        }
    }


    private void handleClassExpressionInd(OWLObject subject, OWLClassExpression expression, int depth) {

        expression.accept(new OWLClassExpressionVisitor() {

            @Override
            public void visit(OWLClass ce) {
                if (subject instanceof OWLNamedIndividual) { // don't show for blank nodes
                    // add type
                    addIfFilterAllows(subject,
                            df.getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE.getIRI()),
                            ce,
                            descr::isAllowedProperty
                    );
                }
            }

            @Override
            public void visit(OWLObjectIntersectionOf ce) {
                ce.operands().forEach(op -> handleClassExpressionInd(subject, op, depth)); // at same level
            }

            @Override
            public void visit(OWLObjectSomeValuesFrom ce) {
                var added = addIfFilterAllows(
                        subject,
                        ce.getProperty().asOWLObjectProperty(),
                        ce.getFiller(),
                        descr::isAllowedProperty
                );
                if (added) {
                    handleClassExpressionInd(ce.getFiller(), ce.getFiller(), depth); // at same level
                }
            }

            @Override
            public void visit(OWLObjectHasValue ce) {
                addIfFilterAllows(
                        subject,
                        ce.getProperty().asOWLObjectProperty(),
                        ce.getFiller(),
                        descr::isAllowedProperty
                );
            }
        });
    }

    private boolean addIfFilterAllows(
            OWLObject subject,
            OWLProperty property,
            OWLObject object,
            Predicate<OWLProperty> isAllowed
    ) {
        var newEdge = new Graph.Edge(subject, property, object);
        if (edges.contains(newEdge)) {
            return false;
        }
        if (descr.isFollowProperty(property)) {
            edges.add(newEdge);
            return true;
        }
        if (isAllowed.test(property)) {
            edges.add(newEdge);
            return true;
        }
        return false;
    }
}
