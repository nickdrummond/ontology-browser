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

    public Zones createZones(OWLEntity entity, ZoneDescriptors zones, OWLOntology ont, OWLEntityFinder finder, ProxyBuilder proxyBuilder) {
        var top = createGraph("top", entity, zones.top(), zones.bottom(), ont, finder);
        var left = createGraph("left", entity, zones.left(), zones.right(), ont, finder);
        var right = createGraph("right", entity, zones.right(), zones.left(), ont, finder);
        var bottom = zones.bottom().isEmpty() // default to all others if not specified
                ? new GraphDescriptor(ont).withoutProperties(top, left, right).addEntity(entity)
                : createGraph("bottom", entity, zones.bottom(), zones.top(), ont, finder);

        return new Zones(
                new GraphBuilder(proxyBuilder, top).build(),
                new GraphBuilder(proxyBuilder, bottom).build(),
                new GraphBuilder(proxyBuilder, left).build(),
                new GraphBuilder(proxyBuilder, right).build());
    }

    private GraphDescriptor createGraph(
            String name,
            OWLEntity entity,
            List<String> propNames,
            List<String> invPropNames,
            OWLOntology ont,
            OWLEntityFinder finder) {

        Set<OWLProperty> props = getProps(propNames, ont, finder);
        Set<OWLProperty> invProps = getProps(invPropNames, ont, finder);

        var builder = new GraphDescriptor(ont).withName(name);
        if (!props.isEmpty() || !invProps.isEmpty()) {
            builder.addEntity(entity).withProperties(props).withInverseProperties(invProps);
        }
        return builder;
    }

    private Set<OWLProperty> getProps(List<String> propNames, OWLOntology ont, OWLEntityFinder finder) {
        return propNames.stream()
                .map(name -> getProps(name, finder, ont))
                .flatMap(Collection::stream)
                .collect(Collectors.toSet());
    }

    private Set<OWLObjectProperty> getProps(String name, OWLEntityFinder finder, OWLOntology ont) {
        if (name.equals("type")){
            var type = ont.getOWLOntologyManager().getOWLDataFactory().getOWLObjectProperty(OWLRDFVocabulary.RDF_TYPE);
            return Set.of(type);
        }
        return finder.getOWLObjectProperties(name);
    }
}
