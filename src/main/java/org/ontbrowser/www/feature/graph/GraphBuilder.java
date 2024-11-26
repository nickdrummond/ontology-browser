package org.ontbrowser.www.feature.graph;

import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.annotation.Nonnull;
import java.util.*;

// TODO could make the properties a list and use that order for display?
public class GraphBuilder {

    private static final Logger log = LoggerFactory.getLogger(GraphBuilder.class);

    private final GraphDescriptor descr;
    private final OWLOntology ont;
    private final OWLDataFactory df;
    private ProxyBuilder proxyBuilder;

    private final Map<Integer, Set<Graph.Edge>> edgesByDepth = new HashMap<>();

    private final Set<Graph.Edge> edges = new HashSet<>();
    private boolean built = false;
    private final Set<OWLNamedIndividual> renderedIndividuals = new HashSet<>();
    private final Set<OWLClass> renderedClasses = new HashSet<>();

    public GraphBuilder(ProxyBuilder proxyBuilder, GraphDescriptor descr) {
        this.descr = descr;
        this.proxyBuilder = proxyBuilder;
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
            descr.getIndividuals().forEach(ind -> buildInd(ind, 0));
            descr.getClasses().forEach(cls -> buildCls(cls, 0));
            built = true;
        }
        return new Graph(edges);
    }

    private void buildInd(OWLNamedIndividual ind, int depth) {
        if (depth > descr.getMaxDepth()) {
            return;
        }

        if (!renderedIndividuals.contains(ind)) {
            renderedIndividuals.add(ind);
            ont.referencingAxioms(ind, Imports.INCLUDED).forEach(ax -> ax.accept(new OWLAxiomVisitor() {
                @Override
                public void visit(@Nonnull OWLObjectPropertyAssertionAxiom axiom) {
                    // Only named for now
                    if (axiom.getSubject().isNamed() && axiom.getProperty().isNamed() && axiom.getObject().isNamed()) {
                        if (axiom.getSubject().equals(ind)) {
                            if (addIfFilterAllows(
                                    depth, axiom.getSubject().asOWLNamedIndividual(),
                                    axiom.getProperty().getNamedProperty(),
                                    axiom.getObject().asOWLNamedIndividual())) {
                                // follow
                                buildInd(axiom.getObject().asOWLNamedIndividual(), depth + 1);
                            }
//                        else {
//                            log.info("axiom ignored {}", axiom);
//                        }
                        } else {
                            // only follow inverse for depth=0
                            if (depth == 0 && addInverseIfFilterAllows(
                                    depth, axiom.getSubject().asOWLNamedIndividual(),
                                    axiom.getProperty().getNamedProperty(),
                                    axiom.getObject().asOWLNamedIndividual())) {
                                // follow
                                buildInd(axiom.getSubject().asOWLNamedIndividual(), depth + 1);
                            }
//                        else {
//                            log.info("inverse ignored {} depth {}", axiom, depth);
//                        }
                        }
                    }
                }

                @Override
                public void visit(OWLClassAssertionAxiom axiom) {
                    if (axiom.getIndividual().equals(ind)) {

                        var cls = axiom.getClassExpression();
                        if (cls.isNamed()) {
                            if (addIfFilterAllows(
                                    depth, axiom.getIndividual().asOWLNamedIndividual(),
                                    df.getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE),
                                    cls.asOWLClass()
                            )) {
                                // follow
                                buildCls(cls.asOWLClass(), depth + 1);
                            }
                        } else { // break down a class expression
                            handleClassExpressionInd(ind, cls, depth + 1);
                        }
                        // TODO inverses
                    }
                }
            }));
        }
    }

    // TODO when superclass is svf (Jedi) or cls is filler of svf - eg pizza (SlicedTomatoTopping)
    private void buildCls(OWLClass cls, int depth) {
//        if (depth > descr.getMaxDepth()) {
//            return;
//        }

        if (!renderedClasses.contains(cls)) {
            renderedClasses.add(cls);
            ont.referencingAxioms(cls, Imports.INCLUDED).forEach(ax -> ax.accept(new OWLAxiomVisitor() {

                @Override
                public void visit(OWLSubClassOfAxiom axiom) {
                    var supercls = axiom.getSuperClass();
                    var subcls = axiom.getSubClass();
                    if (subcls.equals(cls)) {
                        if (supercls.isNamed()) {
                            addIfFilterAllows(
                                    depth, cls,
                                    df.getOWLObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF),
                                    supercls.asOWLClass()
                            );
                        } else { // break down a class expression
                            handleClassExpression(cls, supercls, depth);
                        }
                    } else {
                        if (subcls.isNamed() && supercls.isNamed()) {
                            addIfFilterAllows(
                                    depth, subcls.asOWLClass(),
                                    df.getOWLObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF),
                                    cls
                            );
                        } else {
                            handleClassExpression(cls, subcls, depth);
                        }
                    }
                }
            }));
        }
    }

    private void handleClassExpressionInd(OWLEntity subject, OWLClassExpression cls, int depth) {
//        if (depth > descr.getMaxDepth()) {
//            return;
//        }

        cls.accept(new OWLClassExpressionVisitor() {

            @Override
            public void visit(OWLObjectSomeValuesFrom ce) {
                getNamedNodeFromFiller(ce.getFiller()).ifPresent(cls -> {
                    var proxy = proxyBuilder.createAnonNode(cls);
                    handleClassExpressionInd(proxy, ce.getFiller(), depth); // at same level
                    addIfFilterAllows(
                            depth, subject,
                            ce.getProperty().asOWLObjectProperty(),
                            proxy
                    );
                });
            }

            @Override
            public void visit(OWLObjectHasValue ce) {
                addIfFilterAllows(depth,
                        subject,
                        ce.getProperty().asOWLObjectProperty(),
                        ce.getFiller().asOWLNamedIndividual()
                );
            }

            @Override
            public void visit(OWLObjectIntersectionOf ce) {
                ce.operands().forEach(op -> handleClassExpressionInd(subject, op, depth)); // at same level
            }
        });
    }

    private void handleClassExpression(OWLEntity subject, OWLClassExpression cls, int depth) {
//        if (depth > descr.getMaxDepth()) {
//            return;
//        }

        cls.accept(new OWLClassExpressionVisitor() {

            @Override
            public void visit(OWLObjectSomeValuesFrom ce) {
                getNamedNodeFromFiller(ce.getFiller()).ifPresent(cls -> {
                    handleClassExpression(cls, ce.getFiller(), depth); // at same level
                    addIfFilterAllows(
                            depth, subject,
                            ce.getProperty().asOWLObjectProperty(),
                            cls
                    );
                });
            }

            @Override
            public void visit(OWLObjectIntersectionOf ce) {
                ce.operands().forEach(op -> handleClassExpression(subject, op, depth)); // at same level
            }
        });
    }

    private Optional<OWLClass> getNamedNodeFromFiller(OWLClassExpression filler) {
        return filler.accept(new OWLClassExpressionVisitorEx<>() {
            @Override
            public Optional<OWLClass> visit(OWLObjectIntersectionOf ce) {
                return ce.operands().filter(IsAnonymous::isNamed).findFirst().map(AsOWLClass::asOWLClass);
            }

            @Override
            public Optional<OWLClass> visit(OWLClass ce) {
                return Optional.of(ce);
            }

            @Override
            public Optional<OWLClass> visit(OWLObjectSomeValuesFrom ce) {
                return Optional.of(df.getOWLThing());
            }

            @Override
            public <T> Optional<OWLClass> doDefault(T object) {
                System.err.println("Cannot find named node from " + object);
                return Optional.empty();
            }
        });
    }

    private boolean addIfFilterAllows(int depth, OWLEntity subject, OWLProperty property, OWLEntity object) {
        var newEdge = new Graph.Edge(subject, property, object);
        var edgesAtDepth = edgesByDepth.getOrDefault(depth, new HashSet<>());
        if (descr.isFollowProperty(property)) {
            if (edgesAtDepth.contains(newEdge)) {
                return false;
            }
            edgesAtDepth.add(newEdge);
            edges.add(newEdge);
            return true;
        }
        if (descr.isAllowedProperty(property)) {
            edgesAtDepth.add(newEdge);
            edges.add(newEdge);
            return true;
        }
        return false;
    }

    private boolean addInverseIfFilterAllows(int depth, OWLEntity subject, OWLProperty property, OWLEntity object) {
        var newEdge = new Graph.Edge(subject, property, object);
        var edgesAtDepth = edgesByDepth.getOrDefault(depth, new HashSet<>());
        if (descr.isFollowProperty(property)) {
            if (edges.contains(newEdge)) {
                return false;
            }
            edgesAtDepth.add(newEdge);
            edges.add(newEdge);
            return true;
        }
        if (descr.isAllowedInverseProperty(property)) {
            edgesAtDepth.add(newEdge);
            edges.add(newEdge);
            return true;
        }
        return false;
    }
}
