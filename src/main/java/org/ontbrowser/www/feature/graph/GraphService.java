package org.ontbrowser.www.feature.graph;

import org.ontbrowser.www.kit.OWLEntityFinder;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLProperty;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
public class GraphService {

    public Zones createZones(OWLEntity entity, ZoneDescriptors zoneDescriptors, OWLOntology ont, OWLEntityFinder finder, ProxyBuilder proxyBuilder) {
        var top = createGraph(entity, zoneDescriptors.topWithProperties(), ont, finder, proxyBuilder);
        var left = createGraph(entity, zoneDescriptors.leftWithProperties(), ont, finder, proxyBuilder);
        var right = createGraph(entity, zoneDescriptors.rightWithProperties(), ont, finder, proxyBuilder);
        var bottom = zoneDescriptors.bottomWithProperties().isEmpty() // default to all others if not specified
                ? new GraphBuilder(ont, proxyBuilder).withoutProperties(top, left, right).addEntity(entity)
                : createGraph(entity, zoneDescriptors.bottomWithProperties(), ont, finder, proxyBuilder);
        return new Zones(top.build(), bottom.build(), left.build(), right.build());
    }

    private GraphBuilder createGraph(
            OWLEntity entity,
            List<String> propNames,
            OWLOntology ont,
            OWLEntityFinder finder,
            ProxyBuilder proxyBuilder) {

        Set<OWLProperty> props = propNames.stream()
                .map(name -> getProps(name, finder, ont))
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());

        var builder = new GraphBuilder(ont, proxyBuilder);
        if (!props.isEmpty()) {
            builder.addEntity(entity).withProperties(props);
        }
        return builder;
    }

    private Set<OWLObjectProperty> getProps(String name, OWLEntityFinder finder, OWLOntology ont) {
        if (name.equals("type")){
            var type = ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE);
            return Set.of(type);
        }
        return finder.getOWLObjectProperties(name);
    }
}
