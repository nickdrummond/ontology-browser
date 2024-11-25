package org.ontbrowser.www.feature.graph;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import com.google.common.collect.Streams;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.util.ShortFormProvider;

import java.util.List;
import java.util.Set;

@JsonSerialize
public class CytoscapeGraph {

    private interface Data{}
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private record EdgeData(String label, String source, String target) implements Data {}
    private record NodeData(String id, String label) implements Data {}
    private record ChildData(String id, String label, String parent) implements Data {}
    private record Element(String group, Data data, boolean selected){}

    private List<Element> elements ;

    public CytoscapeGraph(
            Graph graph,
            ShortFormProvider sfp,
            Set<? extends OWLProperty> compoundRelations,
            Set<OWLNamedIndividual> individuals
    ) {
        var childrenNodes = compoundRelations.stream().<Element>mapMulti((rel, consumer) ->
            graph.getEdgesWithPredicate(rel).forEach(edge -> consumer.accept(childFromEntity(sfp, edge.subject(), edge.object(), individuals.contains(edge.subject()))))
        ).distinct();

        var nodes = graph.edges().stream().<Element>mapMulti((edge, consumer) -> {
            consumer.accept(nodeFromEntity(sfp, edge.object(), individuals.contains(edge.object())));
            if (!compoundRelations.contains(edge.predicate())) {
                consumer.accept(nodeFromEntity(sfp, edge.subject(),individuals.contains(edge.subject())));
            }
        }).distinct();

        // TODO replace any nodes that have a child with a

        var edges = graph.edges().stream()
                .filter(edge -> !compoundRelations.contains(edge.predicate()))
                .map(edge -> transformEdge(edge, sfp));

        elements = Streams.concat(childrenNodes, nodes, edges).toList();
    }

    private Element transformEdge(Graph.Edge edge, ShortFormProvider sfp) {
        String label = sfp.getShortForm(edge.predicate());
        return new Element("edges", new EdgeData(label, getId(edge.subject()), getId(edge.object())), false);
    }

    private static Element nodeFromEntity(ShortFormProvider sfp, OWLEntity subject, boolean selected) {
        String label = sfp.getShortForm(subject);
        return new Element("nodes", new NodeData(getId(subject), label), selected);
    }

    private static Element childFromEntity(ShortFormProvider sfp, OWLEntity subject, OWLEntity parent, boolean selected) {
        String label = sfp.getShortForm(subject);
        return new Element("nodes", new ChildData(getId(subject), label, getId(parent)), selected);
    }

    private static String getId(OWLEntity subject) {
        return subject.getIRI().toString();
    }

    public List<Element> getElements() {
        return elements;
    }
}
