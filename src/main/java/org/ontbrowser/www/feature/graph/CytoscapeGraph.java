package org.ontbrowser.www.feature.graph;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Streams;
import org.ontbrowser.www.renderer.MOSStringRenderer;
import org.semanticweb.owlapi.model.*;

import java.util.List;
import java.util.Set;

@JsonSerialize
public class CytoscapeGraph {

    private final MOSStringRenderer mos;

    private interface Data{}
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private record EdgeData(String label, String source, String target) implements Data {}
    private record NodeData(String id, String label, String type) implements Data {}
    private record ChildData(String id, String label, String parent) implements Data {}
    private record Element(String group, Data data, boolean selected){}

    private final List<Element> elements ;

    public CytoscapeGraph(
            Graph graph,
            MOSStringRenderer mos,
            Set<? extends OWLProperty> parentProperties,
            Set<? extends OWLObject> owlObjects
    ) {
        this.mos = mos;

        var childrenNodes = parentProperties.stream().<Element>mapMulti((rel, consumer) ->
            graph.getEdgesWithPredicate(rel).forEach(edge ->
                    consumer.accept(childFrom(edge.subject(), edge.object(), owlObjects.contains(edge.subject()))))
        ).distinct();

        var nodes = graph.edges().stream().<Element>mapMulti((edge, consumer) -> {
            consumer.accept(nodeFromEntity(edge.object(), owlObjects.contains(edge.object())));
            if (!parentProperties.contains(edge.predicate())) {
                consumer.accept(nodeFromEntity(edge.subject(),owlObjects.contains(edge.subject())));
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

    private Element nodeFromEntity(OWLObject subject, boolean selected) {
        String label = mos.render(subject);
        return new Element("nodes", new NodeData(getId(subject), label, typeFor(subject)), selected);
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
}
