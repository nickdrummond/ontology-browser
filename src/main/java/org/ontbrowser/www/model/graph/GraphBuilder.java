package org.ontbrowser.www.model.graph;

import org.apache.commons.lang3.NotImplementedException;
import org.semanticweb.owlapi.model.*;
import org.semanticweb.owlapi.model.parameters.Imports;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import javax.annotation.Nonnull;
import java.util.*;

// TODO could make the properties a list and use that order for display?
public class GraphBuilder {

    private final OWLOntology ont;
    private final OWLDataFactory df;
    private ProxyBuilder proxyBuilder;

    private final Set<Graph.Edge> edges = new HashSet<>();
    private final Set<OWLProperty> withProperties = new HashSet<>();
    private final Set<OWLProperty> withoutProperties = new HashSet<>();
    private final Set<OWLNamedIndividual> individuals = new HashSet<>();
    private final Set<OWLClass> classes = new HashSet<>();
    private boolean built = false;

    public GraphBuilder(OWLOntology ont, ProxyBuilder proxyBuilder) {
        this.ont = ont;
        this.proxyBuilder = proxyBuilder;
        this.df = ont.getOWLOntologyManager().getOWLDataFactory();
    }

    public GraphBuilder withProperty(OWLProperty property) {
        this.withProperties.add(property);
        return this;
    }

    public GraphBuilder withProperties(OWLProperty... properties) {
        this.withProperties.addAll(Arrays.asList(properties));
        return this;
    }

    public GraphBuilder withProperties(Set<OWLProperty> properties) {
        this.withProperties.addAll(properties);
        return this;
    }

    public GraphBuilder withoutProperty(OWLProperty property) {
        this.withoutProperties.add(property);
        return this;
    }

    public GraphBuilder withoutProperties(OWLProperty... properties) {
        this.withoutProperties.addAll(Arrays.asList(properties));
        return this;
    }

    public GraphBuilder withoutProperties(GraphBuilder... otherBuilders) {
        Arrays.asList(otherBuilders).forEach(builder -> this.withoutProperties.addAll(builder.getProperties()));
        return this;
    }

    private Set<OWLProperty> getProperties() {
        return withProperties;
    }

    public GraphBuilder addEntity(OWLEntity entity) {
        if (entity instanceof OWLNamedIndividual ind) {
            return addIndividual(ind);
        }
        if (entity instanceof OWLClass cls) {
            return addClass(cls);
        }
        throw new NotImplementedException("Unsupported graph type " + entity.getClass());
    }

    public GraphBuilder addIndividual(OWLNamedIndividual ind) {
        this.individuals.add(ind);
        return this;
    }

    public GraphBuilder addClass(OWLClass cls) {
        this.classes.add(cls);
        return this;
    }

    // Add arbitrary edges
    public GraphBuilder add(Graph.Edge edge) {
        this.edges.add(edge);
        return this;
    }

    public Graph build() {
        if (!built) {
            individuals.forEach(this::buildInd);
            classes.forEach(this::buildCls);
            built = true;
        }
        return new Graph(edges);
    }

    private void buildInd(OWLNamedIndividual ind) {
        ont.referencingAxioms(ind, Imports.INCLUDED).forEach(ax -> ax.accept(new OWLAxiomVisitor() {
            @Override
            public void visit(@Nonnull OWLObjectPropertyAssertionAxiom axiom) {
                // Only named for now
                if (axiom.getSubject().isNamed() && axiom.getProperty().isNamed() && axiom.getObject().isNamed()) {
                    addIfFilterAllows(
                            0, axiom.getSubject().asOWLNamedIndividual(),
                            axiom.getProperty().getNamedProperty(),
                            axiom.getObject().asOWLNamedIndividual());
                }
            }

            @Override
            public void visit(OWLClassAssertionAxiom axiom) {
                if (axiom.getIndividual().equals(ind)) {

                    var cls = axiom.getClassExpression();
                    if (cls.isNamed()) {
                        addIfFilterAllows(
                                0, axiom.getIndividual().asOWLNamedIndividual(),
                                df.getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE),
                                cls.asOWLClass()
                        );
                    }
                    else { // break down a class expression
                        handleClassExpressionInd(ind, cls, 0);
                    }
                }
            }
        }));
    }

    // TODO when superclass is svf (Jedi) or cls is filler of svf - eg pizza (SlicedTomatoTopping)
    private void buildCls(OWLClass cls) {
        ont.referencingAxioms(cls, Imports.INCLUDED).forEach(ax -> ax.accept(new OWLAxiomVisitor() {

            @Override
            public void visit(OWLSubClassOfAxiom axiom) {
                var supercls = axiom.getSuperClass();
                var subcls = axiom.getSubClass();
                if (subcls.equals(cls)) {
                    if (supercls.isNamed()) {
                        addIfFilterAllows(
                                0, cls,
                                df.getOWLObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF),
                                supercls.asOWLClass()
                        );
                    }
                    else { // break down a class expression
                        handleClassExpression(cls, supercls, 0);
                    }
                }
                else {
                    if (subcls.isNamed() && supercls.isNamed()) {
                        addIfFilterAllows(
                                0, subcls.asOWLClass(),
                                df.getOWLObjectProperty(OWLRDFVocabulary.RDFS_SUBCLASS_OF),
                                cls
                        );
                    }
                    else {
                        handleClassExpression(cls, subcls, 0);
                    }
                }
            }
        }));
    }

    private void handleClassExpressionInd(OWLEntity subject, OWLClassExpression cls, int depth) {
        cls.accept(new OWLClassExpressionVisitor() {

            @Override
            public void visit(OWLObjectSomeValuesFrom ce) {
                getNamedNodeFromFiller(ce.getFiller()).ifPresent(cls -> {
                    var proxy = proxyBuilder.createAnonNode(cls);
                    handleClassExpressionInd(proxy, ce.getFiller(), depth+1);
                    addIfFilterAllows(
                            depth, subject,
                            ce.getProperty().asOWLObjectProperty(),
                            proxy
                    );
                });
            }

            @Override
            public void visit(OWLObjectHasValue ce) {
                addIfFilterAllows( depth,
                        subject,
                        ce.getProperty().asOWLObjectProperty(),
                        ce.getFiller().asOWLNamedIndividual()
                );
            }

            @Override
            public void visit(OWLObjectIntersectionOf ce) {
                ce.operands().forEach(op -> handleClassExpressionInd(subject, op, depth+1));
            }
        });
    }

    private void handleClassExpression(OWLEntity subject, OWLClassExpression cls, int depth) {
        cls.accept(new OWLClassExpressionVisitor() {

            @Override
            public void visit(OWLObjectSomeValuesFrom ce) {
                getNamedNodeFromFiller(ce.getFiller()).ifPresent(cls -> {
                    handleClassExpression(cls, ce.getFiller(), depth+1);
                    addIfFilterAllows(
                            depth, subject,
                            ce.getProperty().asOWLObjectProperty(),
                            cls
                    );
                });
            }

            @Override
            public void visit(OWLObjectIntersectionOf ce) {
                ce.operands().forEach(op -> handleClassExpression(subject, op, depth+1));
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

    private void addIfFilterAllows(int depth, OWLEntity subject, OWLProperty property, OWLEntity object) {
        if (depth > 0 || isAllowedProperty(property)) {
            add(new Graph.Edge(subject, property, object));
        }
    }

    private boolean isAllowedProperty(OWLProperty property) {
        return (withProperties.isEmpty() || withProperties.contains(property)) && !withoutProperties.contains(property);
    }
}
