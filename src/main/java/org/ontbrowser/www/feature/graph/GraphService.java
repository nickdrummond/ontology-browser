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
        var top = createGraph("top", entity, zones.top(), zones.bottom(), ont, finder, proxyBuilder);
        var left = createGraph("left", entity, zones.left(), zones.right(), ont, finder, proxyBuilder);
        var right = createGraph("right", entity, zones.right(), zones.left(), ont, finder, proxyBuilder);
        var bottom = zones.bottom().isEmpty() // default to all others if not specified
                ? new GraphBuilder(ont, proxyBuilder).withoutProperties(top, left, right).addEntity(entity)
                : createGraph("bottom", entity, zones.bottom(), zones.top(), ont, finder, proxyBuilder);
        return new Zones(top.build(), bottom.build(), left.build(), right.build());
    }

    private GraphBuilder createGraph(
            String name,
            OWLEntity entity,
            List<String> propNames,
            List<String> invPropNames,
            OWLOntology ont,
            OWLEntityFinder finder,
            ProxyBuilder proxyBuilder) {

        Set<OWLProperty> props = getProps(propNames, ont, finder);
        Set<OWLProperty> invProps = getProps(invPropNames, ont, finder);

        var builder = new GraphBuilder(ont, proxyBuilder).withName(name);
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
