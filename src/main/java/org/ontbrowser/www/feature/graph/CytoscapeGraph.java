package org.ontbrowser.www.feature.graph;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Streams;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.semanticweb.owlapi.model.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Optional;
import java.util.Set;

@JsonSerialize
public class CytoscapeGraph {

    private static final Logger log = LoggerFactory.getLogger(CytoscapeGraph.class);

    private final MOSStringRenderer mos;
    private final OWLDataFactory df;

    private interface Data{}
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private record EdgeData(int id, String label, int source, int target) implements Data {}
    private record NodeData(int id, String label, String type) implements Data {}
    private record ChildData(int id, String label, int parent) implements Data {}
    private record Element(String group, Data data, boolean selected){}

    private final List<Element> elements ;

    public CytoscapeGraph(
            Graph graph,
            OWLDataFactory df,
            MOSStringRenderer mos,
            Set<? extends OWLProperty> parentProperties,
            Set<? extends OWLObject> owlObjects
    ) {
        this.mos = mos;
        this.df =df;

        var childrenNodes = parentProperties.stream().<Element>mapMulti((rel, consumer) ->
            graph.getEdgesWithPredicate(rel).forEach(edge ->
                    consumer.accept(childFrom(edge.subject(), edge.object(), owlObjects.contains(edge.subject()))))
        );

        var nodes = graph.edges().stream().<Element>mapMulti((edge, consumer) -> {
            consumer.accept(nodeFrom(edge.object(), owlObjects.contains(edge.object())));
            if (!parentProperties.contains(edge.predicate())) {
                consumer.accept(nodeFrom(edge.subject(),owlObjects.contains(edge.subject())));
            }
        });

        var edges = graph.edges().stream()
                .filter(edge -> !parentProperties.contains(edge.predicate()))
                .map(this::transformEdge);

//        // If any of the nodes are numeric, add a relation
//        Stream<Element> numericEdges = IntStream.range(-32, 35)
//                .mapToObj(this::numericEdge)
//                .flatMap(this::fillInNumericNodes);
//
        elements = Streams.concat(
                childrenNodes,
                nodes,
                edges
//                numericEdges
        ).distinct().toList();
    }

//    private Stream<Element> fillInNumericNodes(EdgeData edgeData) {
//        return Stream.of(
//                new Element("edges", edgeData, false),
//                new Element("nodes", new NodeData(edgeData.source.hashCode(), edgeData.source, "literal"), false),
//                new Element("nodes", new NodeData(edgeData.target.hashCode(), edgeData.target, "literal"), false)
//        );
//    }

//    private EdgeData numericEdge(int i) {
//        return new EdgeData("after", String.valueOf(i), String.valueOf(i - 1));
//    }

    private Element transformEdge(Graph.Edge edge) {
        String label = mos.render(edge.predicate());
        int subjectId = getId(edge.subject());
        int objectId = getId(edge.object());
        // Combine the predicate and nodes in a hash to get a unique ID for the edge
        EdgeData data = new EdgeData((label+subjectId+objectId).hashCode(), label, subjectId, objectId);
        return new Element("edges", data, false);
    }

    private Element nodeFrom(OWLObject subject, boolean selected) {
        var label = getLabel(subject);
        return new Element("nodes", new NodeData(getId(subject), label, typeFor(subject)), selected);
    }

    private String getLabel(OWLObject subject) {
        String label = null;
        if (subject instanceof OWLObjectIntersectionOf intersection) {
            var namedCls = getNamedNodeFromFiller(intersection);
            if (namedCls.isPresent()) {
                label = mos.render(namedCls.get());
            }
        }
        if (label == null) {
            label = mos.render(subject);
        }
        return label;
    }

    private String typeFor(OWLObject subject) {
        if (subject instanceof OWLNamedIndividual) {
            return "individual";
        }
        else if (subject instanceof OWLClass) {
            return "class";
        }
        else if (subject instanceof OWLLiteral) {
            return "literal";
        }
        return "expression";
    }

    private Element childFrom(OWLObject subject, OWLObject parent, boolean selected) {
        String label = mos.render(subject);
        return new Element("nodes", new ChildData(getId(subject), label, getId(parent)), selected);
    }

    private int getId(OWLObject subject) {
        if (subject instanceof OWLEntity entity) {
            return entity.getIRI().toString().hashCode();
        }
        return mos.render(subject).hashCode();
    }

    public List<Element> getElements() {
        return elements;
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
            public <T> Optional<OWLClass> doDefault(T object) {
                return Optional.of(df.getOWLThing());
            }
        });
    }
}
