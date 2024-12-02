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
    private record EdgeData(String label, String source, String target) implements Data {}
    private record NodeData(String id, String label, String type) implements Data {}
    private record ChildData(String id, String label, String parent) implements Data {}
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
        ).distinct();

        var nodes = graph.edges().stream().<Element>mapMulti((edge, consumer) -> {
            consumer.accept(nodeFrom(edge.object(), owlObjects.contains(edge.object())));
            if (!parentProperties.contains(edge.predicate())) {
                consumer.accept(nodeFrom(edge.subject(),owlObjects.contains(edge.subject())));
            }
        }).distinct();

        var edges = graph.edges().stream()
                .filter(edge -> !parentProperties.contains(edge.predicate()))
                .map(this::transformEdge);

        elements = Streams.concat(childrenNodes, nodes, edges).toList();
    }

    private Element transformEdge(Graph.Edge edge) {
        EdgeData data = new EdgeData(mos.render(edge.predicate()), getId(edge.subject()), getId(edge.object()));
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
        return "expression";
    }

    private Element childFrom(OWLObject subject, OWLObject parent, boolean selected) {
        String label = mos.render(subject);
        return new Element("nodes", new ChildData(getId(subject), label, getId(parent)), selected);
    }

    private String getId(OWLObject subject) {
        if (subject instanceof OWLEntity entity) {
            return entity.getIRI().toString();
        }
        return mos.render(subject);
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
